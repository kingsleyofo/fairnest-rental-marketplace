;; fairnest-governance
;; 
;; This contract implements the governance system for the FairNest rental marketplace,
;; enabling stakeholders to propose and vote on platform changes and participate in
;; dispute resolution. The governance model uses a weighted voting system based on
;; user reputation and platform activity to ensure fair representation while
;; protecting against potential attacks.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-VOTING-PERIOD-ENDED (err u102))
(define-constant ERR-VOTING-PERIOD-ACTIVE (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u105))
(define-constant ERR-INVALID-PARAMETER (err u106))
(define-constant ERR-DISPUTE-NOT-FOUND (err u107))
(define-constant ERR-NOT-ARBITRATOR (err u108))
(define-constant ERR-DISPUTE-ALREADY-RESOLVED (err u109))
(define-constant ERR-PROPOSAL-IMPLEMENTATION-FAILED (err u110))

;; Platform parameters
(define-data-var min-proposal-threshold uint u100)
(define-data-var voting-period-length uint u14400) ;; Default: 10 days (in blocks)
(define-data-var execution-delay uint u1440) ;; Default: 1 day (in blocks)
(define-data-var arbitrator-count uint u7) ;; Number of arbitrators selected for each dispute
(define-data-var min-reputation-for-arbitrator uint u80) ;; Minimum reputation score to be eligible as arbitrator

;; Data structures

;; Proposal types mapping
(define-map proposal-types
  { type-id: uint }
  { 
    name: (string-ascii 50),
    description: (string-utf8 500)
  }
)

;; Stores proposals
(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-utf8 100),
    description: (string-utf8 1000),
    proposal-type: uint,
    parameter-key: (optional (string-ascii 50)),
    parameter-value: (optional (string-utf8 500)),
    created-at-block: uint,
    voting-ends-at-block: uint,
    execution-at-block: uint,
    status: (string-ascii 20), ;; "active", "passed", "rejected", "executed", "failed"
    yes-votes: uint,
    no-votes: uint,
    abstain-votes: uint
  }
)

;; Tracks who has voted on which proposals
(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { 
    vote: (string-ascii 10), ;; "yes", "no", "abstain"
    voting-power: uint
  }
)

;; Stores dispute cases
(define-map disputes
  { dispute-id: uint }
  {
    created-by: principal,
    rental-id: uint,
    description: (string-utf8 1000),
    evidence-hash: (buff 32), ;; IPFS or similar hash of evidence files
    created-at-block: uint,
    resolved: bool,
    resolution: (optional (string-utf8 500)),
    in-favor-of: (optional principal)
  }
)

;; Tracks arbitrators assigned to disputes
(define-map dispute-arbitrators
  { dispute-id: uint, arbitrator: principal }
  {
    vote: (optional (string-ascii 20)), ;; "landlord", "tenant", "split", "dismiss"
    reasoning: (optional (string-utf8 500))
  }
)

;; Counters
(define-data-var proposal-id-counter uint u0)
(define-data-var dispute-id-counter uint u0)

;; System configuration
(define-data-var admin principal tx-sender)

;; Private functions

;; Get a user's voting power based on their reputation and platform activity
(define-private (get-voting-power (user principal))
  ;; Simplified implementation - in production this would call into a reputation contract
  ;; and consider factors like active rentals, transaction history, etc.
  ;; For now we'll return a placeholder value
  u10
)

;; Check if proposal exists
(define-private (is-proposal-exist (proposal-id uint))
  (is-some (map-get? proposals { proposal-id: proposal-id }))
)

;; Update proposal status based on voting results
(define-private (finalize-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) false))
    (yes-votes (get yes-votes proposal))
    (no-votes (get no-votes proposal))
    (total-votes (+ yes-votes no-votes))
    (new-status (if (> yes-votes no-votes) "passed" "rejected"))
  )
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { status: new-status })
    )
    true
  )
)

;; Execute a passed proposal
(define-private (execute-proposal-internal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) false))
    (proposal-type (get proposal-type proposal))
    (param-key (get parameter-key proposal))
    (param-value (get parameter-value proposal))
  )
    ;; Different execution logic based on proposal type
    (if (is-eq proposal-type u1)
      ;; Platform parameter change
      (match (try! (update-platform-parameter param-key param-value))
        success (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal { status: "executed" })
        )
        error (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal { status: "failed" })
        )
      )
      ;; For other proposal types, implement their specific logic
      false
    )
  )
)

;; Helper to update platform parameters
(define-private (update-platform-parameter (param-key (optional (string-ascii 50))) (param-value (optional (string-utf8 500))))
  (match param-key
    key (match param-value
      value (begin
        (if (is-eq key "min-proposal-threshold")
          (var-set min-proposal-threshold (unwrap! (string-to-uint value) ERR-INVALID-PARAMETER))
          (if (is-eq key "voting-period-length")
            (var-set voting-period-length (unwrap! (string-to-uint value) ERR-INVALID-PARAMETER))
            (if (is-eq key "execution-delay")
              (var-set execution-delay (unwrap! (string-to-uint value) ERR-INVALID-PARAMETER))
              (if (is-eq key "arbitrator-count")
                (var-set arbitrator-count (unwrap! (string-to-uint value) ERR-INVALID-PARAMETER))
                (if (is-eq key "min-reputation-for-arbitrator")
                  (var-set min-reputation-for-arbitrator (unwrap! (string-to-uint value) ERR-INVALID-PARAMETER))
                  ERR-INVALID-PARAMETER
                )
              )
            )
          )
        )
        (ok true)
      ))
      ERR-INVALID-PARAMETER
    )
    ERR-INVALID-PARAMETER
  )
)

;; Helper to select arbitrators for a dispute
(define-private (select-arbitrators (dispute-id uint))
  ;; In production, this would select random arbitrators based on reputation
  ;; For now, this is a placeholder
  true
)

;; Read-only functions

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get user's vote on a proposal
(define-read-only (get-user-vote (proposal-id uint) (user principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: user })
)

;; Check if voting is active for a proposal
(define-read-only (is-voting-active (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (< block-height (get voting-ends-at-block proposal))
    false
  )
)

;; Get dispute details
(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

;; Get arbitrator's vote on a dispute
(define-read-only (get-arbitrator-vote (dispute-id uint) (arbitrator principal))
  (map-get? dispute-arbitrators { dispute-id: dispute-id, arbitrator: arbitrator })
)

;; Get current platform parameters
(define-read-only (get-platform-parameters)
  {
    min-proposal-threshold: (var-get min-proposal-threshold),
    voting-period-length: (var-get voting-period-length),
    execution-delay: (var-get execution-delay),
    arbitrator-count: (var-get arbitrator-count),
    min-reputation-for-arbitrator: (var-get min-reputation-for-arbitrator)
  }
)

;; Public functions

;; Create a new governance proposal
(define-public (create-proposal (title (string-utf8 100)) (description (string-utf8 1000)) 
                               (proposal-type uint) 
                               (parameter-key (optional (string-ascii 50))) 
                               (parameter-value (optional (string-utf8 500))))
  (let (
    (user-voting-power (get-voting-power tx-sender))
    (proposal-id (+ (var-get proposal-id-counter) u1))
    (current-block block-height)
    (voting-end-block (+ current-block (var-get voting-period-length)))
    (execution-block (+ voting-end-block (var-get execution-delay)))
  )
    ;; Check if user has enough voting power to create a proposal
    (asserts! (>= user-voting-power (var-get min-proposal-threshold)) ERR-INSUFFICIENT-VOTING-POWER)
    
    ;; Create the proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        proposal-type: proposal-type,
        parameter-key: parameter-key,
        parameter-value: parameter-value,
        created-at-block: current-block,
        voting-ends-at-block: voting-end-block,
        execution-at-block: execution-block,
        status: "active",
        yes-votes: u0,
        no-votes: u0,
        abstain-votes: u0
      }
    )
    
    ;; Update the counter
    (var-set proposal-id-counter proposal-id)
    
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-type (string-ascii 10)))
  (let (
    (user-voting-power (get-voting-power tx-sender))
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
  )
    ;; Verify vote-type is valid
    (asserts! (or (is-eq vote-type "yes") (is-eq vote-type "no") (is-eq vote-type "abstain")) ERR-INVALID-PARAMETER)
    
    ;; Check if voting period is still active
    (asserts! (< block-height (get voting-ends-at-block proposal)) ERR-VOTING-PERIOD-ENDED)
    
    ;; Check if user has already voted
    (asserts! (is-none (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    ;; Record the vote
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: tx-sender }
      { 
        vote: vote-type,
        voting-power: user-voting-power
      }
    )
    
    ;; Update vote counts in the proposal
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal 
        (if (is-eq vote-type "yes")
          { yes-votes: (+ (get yes-votes proposal) user-voting-power) }
          (if (is-eq vote-type "no")
            { no-votes: (+ (get no-votes proposal) user-voting-power) }
            { abstain-votes: (+ (get abstain-votes proposal) user-voting-power) }
          )
        )
      )
    )
    
    (ok true)
  )
)

;; Finalize a proposal after voting period ends
(define-public (finalize-proposal-vote (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
  )
    ;; Check that voting period has ended
    (asserts! (>= block-height (get voting-ends-at-block proposal)) ERR-VOTING-PERIOD-ACTIVE)
    
    ;; Check that proposal is still in active status
    (asserts! (is-eq (get status proposal) "active") ERR-PROPOSAL-NOT-FOUND)
    
    ;; Update proposal status based on vote results
    (asserts! (finalize-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND)
    
    (ok true)
  )
)

;; Execute a passed proposal after the execution delay
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
  )
    ;; Check that proposal has passed and execution delay has elapsed
    (asserts! (is-eq (get status proposal) "passed") ERR-PROPOSAL-NOT-FOUND)
    (asserts! (>= block-height (get execution-at-block proposal)) ERR-VOTING-PERIOD-ACTIVE)
    
    ;; Try to execute the proposal
    (if (execute-proposal-internal proposal-id)
      (ok true)
      ERR-PROPOSAL-IMPLEMENTATION-FAILED
    )
  )
)

;; Create a new dispute case
(define-public (create-dispute (rental-id uint) (description (string-utf8 1000)) (evidence-hash (buff 32)))
  (let (
    (dispute-id (+ (var-get dispute-id-counter) u1))
  )
    ;; Create the dispute
    (map-set disputes
      { dispute-id: dispute-id }
      {
        created-by: tx-sender,
        rental-id: rental-id,
        description: description,
        evidence-hash: evidence-hash,
        created-at-block: block-height,
        resolved: false,
        resolution: none,
        in-favor-of: none
      }
    )
    
    ;; Update the counter
    (var-set dispute-id-counter dispute-id)
    
    ;; Select arbitrators for this dispute
    (select-arbitrators dispute-id)
    
    (ok dispute-id)
  )
)

;; Submit an arbitrator's vote on a dispute
(define-public (submit-arbitration-vote (dispute-id uint) (vote (string-ascii 20)) (reasoning (string-utf8 500)))
  (let (
    (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
    (arbitrator-record (unwrap! (map-get? dispute-arbitrators { dispute-id: dispute-id, arbitrator: tx-sender }) ERR-NOT-ARBITRATOR))
  )
    ;; Check that dispute is not already resolved
    (asserts! (not (get resolved dispute)) ERR-DISPUTE-ALREADY-RESOLVED)
    
    ;; Verify vote is valid
    (asserts! (or (is-eq vote "landlord") (is-eq vote "tenant") (is-eq vote "split") (is-eq vote "dismiss")) ERR-INVALID-PARAMETER)
    
    ;; Record the arbitrator's vote
    (map-set dispute-arbitrators
      { dispute-id: dispute-id, arbitrator: tx-sender }
      (merge arbitrator-record { 
        vote: (some vote),
        reasoning: (some reasoning)
      })
    )
    
    (ok true)
  )
)

;; Finalize a dispute resolution based on arbitrator votes
(define-public (resolve-dispute (dispute-id uint))
  (let (
    (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
  )
    ;; Check that dispute is not already resolved
    (asserts! (not (get resolved dispute)) ERR-DISPUTE-ALREADY-RESOLVED)
    
    ;; Only the admin can resolve disputes for now
    ;; In a more decentralized version, this would tally arbitrator votes automatically
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    
    ;; Resolution logic would go here - for now just mark as resolved
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute { 
        resolved: true,
        resolution: (some "Resolution determined by administrator"),
        in-favor-of: none
      })
    )
    
    (ok true)
  )
)

;; Administrative function to update platform parameters directly (emergency use only)
(define-public (admin-update-parameter (param-key (string-ascii 50)) (param-value (string-utf8 500)))
  (begin
    ;; Only the admin can update parameters directly
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    
    (try! (update-platform-parameter (some param-key) (some param-value)))
    
    (ok true)
  )
)

;; Initialize proposal types
(map-set proposal-types { type-id: u1 } { name: "Platform Parameter", description: "Change a platform configuration parameter" })
(map-set proposal-types { type-id: u2 } { name: "Feature Implementation", description: "Implement a new platform feature" })
(map-set proposal-types { type-id: u3 } { name: "Emergency Action", description: "Address a critical platform issue" })