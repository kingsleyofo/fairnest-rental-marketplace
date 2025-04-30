;; fairnest-marketplace
;; A decentralized rental marketplace that manages property listings, rental agreements,
;; payments, security deposits, and reputation for property owners and renters.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LISTING-NOT-FOUND (err u101))
(define-constant ERR-INVALID-LISTING-DATA (err u102))
(define-constant ERR-LISTING-NOT-ACTIVE (err u103))
(define-constant ERR-APPLICATION-NOT-FOUND (err u104))
(define-constant ERR-APPLICATION-ALREADY-PROCESSED (err u105))
(define-constant ERR-RENTAL-AGREEMENT-NOT-FOUND (err u106))
(define-constant ERR-INSUFFICIENT-FUNDS (err u107))
(define-constant ERR-INVALID-PAYMENT (err u108))
(define-constant ERR-INVALID-DATES (err u109))
(define-constant ERR-ALREADY-LISTED (err u110))
(define-constant ERR-DISPUTE-EXISTS (err u111))
(define-constant ERR-NO-ACTIVE-RENTAL (err u112))
(define-constant ERR-INVALID-REVIEW-SCORE (err u113))
(define-constant ERR-RENTAL-NOT-COMPLETED (err u114))
(define-constant ERR-RENTAL-NOT-ACTIVE (err u115))
(define-constant ERR-NOT-PARTICIPANT (err u116))

;; Platform configuration
(define-constant PLATFORM-FEE-PERCENT u3)  ;; 3% platform fee
(define-constant PLATFORM-ADDRESS 'SP000000000000000000002Q6VF78)  ;; Replace with actual platform address
(define-constant MAX-REVIEW-SCORE u5)  ;; Reviews on scale of 1-5

;; Status constants
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-INACTIVE u2)
(define-constant STATUS-PENDING u3)
(define-constant STATUS-APPROVED u4)
(define-constant STATUS-REJECTED u5)
(define-constant STATUS-COMPLETED u6)
(define-constant STATUS-DISPUTED u7)

;; Data structures

;; Property listing information
(define-map property-listings
  { listing-id: uint }
  {
    owner: principal,
    title: (string-ascii 100),
    description: (string-utf8 500),
    location: (string-ascii 100),
    price-per-night: uint,
    security-deposit: uint,
    min-nights: uint,
    max-nights: uint,
    amenities: (list 20 (string-ascii 50)),
    availability-start: uint,  ;; Unix timestamp
    availability-end: uint,    ;; Unix timestamp
    status: uint,
    created-at: uint
  }
)

;; Track the next available listing ID
(define-data-var next-listing-id uint u1)

;; Rental applications submitted by potential renters
(define-map rental-applications
  { application-id: uint }
  {
    listing-id: uint,
    applicant: principal,
    start-date: uint,          ;; Unix timestamp
    end-date: uint,            ;; Unix timestamp
    message: (string-utf8 300),
    status: uint,
    created-at: uint
  }
)

;; Track the next available application ID
(define-data-var next-application-id uint u1)

;; Active rental agreements
(define-map rental-agreements
  { agreement-id: uint }
  {
    listing-id: uint,
    owner: principal,
    renter: principal,
    start-date: uint,
    end-date: uint,
    total-price: uint,
    security-deposit: uint,
    platform-fee: uint,
    status: uint,
    created-at: uint
  }
)

;; Track the next available agreement ID
(define-data-var next-agreement-id uint u1)

;; Map listing IDs to their active agreement IDs (if any)
(define-map listing-to-agreement
  { listing-id: uint }
  { agreement-id: uint }
)

;; Disputes raised for rental agreements
(define-map disputes
  { agreement-id: uint }
  {
    raised-by: principal,
    reason: (string-utf8 500),
    status: uint,
    resolution: (optional (string-utf8 500)),
    created-at: uint
  }
)

;; User reputation and reviews
(define-map user-reputation
  { user: principal }
  {
    total-score: uint,
    review-count: uint,
    as-owner-score: uint,
    as-owner-count: uint,
    as-renter-score: uint,
    as-renter-count: uint
  }
)

;; Individual reviews
(define-map reviews
  { review-id: uint }
  {
    agreement-id: uint,
    reviewer: principal,
    reviewee: principal,
    score: uint,
    comment: (string-utf8 300),
    reviewer-type: (string-ascii 10),  ;; "owner" or "renter"
    created-at: uint
  }
)

;; Track the next available review ID
(define-data-var next-review-id uint u1)

;; Map to check if a review has already been submitted for an agreement by a user
(define-map agreement-reviews
  { agreement-id: uint, reviewer: principal }
  { review-id: uint }
)

;; Private functions

;; Calculate the total price for a rental period
(define-private (calculate-total-price (price-per-night uint) (start-date uint) (end-date uint))
  (let
    (
      (nights (/ (- end-date start-date) u86400))  ;; Convert seconds to days
      (total (* price-per-night nights))
    )
    total
  )
)

;; Calculate the platform fee
(define-private (calculate-platform-fee (total-price uint))
  (/ (* total-price PLATFORM-FEE-PERCENT) u100)
)

;; Validate listing dates
(define-private (validate-dates (start-date uint) (end-date uint))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time u0)))
    )
    (and
      (> start-date current-time)
      (> end-date start-date)
    )
  )
)

;; Update user reputation with a new review
(define-private (update-reputation (user principal) (score uint) (is-owner bool))
  (let
    (
      (current-rep (default-to
        {
          total-score: u0,
          review-count: u0,
          as-owner-score: u0,
          as-owner-count: u0,
          as-renter-score: u0,
          as-renter-count: u0
        }
        (map-get? user-reputation { user: user })
      ))
      (new-total-score (+ (get total-score current-rep) score))
      (new-review-count (+ (get review-count current-rep) u1))
    )
    (if is-owner
      (map-set user-reputation
        { user: user }
        {
          total-score: new-total-score,
          review-count: new-review-count,
          as-owner-score: (+ (get as-owner-score current-rep) score),
          as-owner-count: (+ (get as-owner-count current-rep) u1),
          as-renter-score: (get as-renter-score current-rep),
          as-renter-count: (get as-renter-count current-rep)
        }
      )
      (map-set user-reputation
        { user: user }
        {
          total-score: new-total-score,
          review-count: new-review-count,
          as-owner-score: (get as-owner-score current-rep),
          as-owner-count: (get as-owner-count current-rep),
          as-renter-score: (+ (get as-renter-score current-rep) score),
          as-renter-count: (+ (get as-renter-count current-rep) u1)
        }
      )
    )
  )
)

;; Check if a user is authorized to view sensitive information or perform actions
(define-private (is-participant (owner principal) (renter principal))
  (or
    (is-eq tx-sender owner)
    (is-eq tx-sender renter)
  )
)

;; Read-only functions

;; Get property listing details
(define-read-only (get-listing (listing-id uint))
  (map-get? property-listings { listing-id: listing-id })
)

;; Get application details
(define-read-only (get-application (application-id uint))
  (map-get? rental-applications { application-id: application-id })
)

;; Get rental agreement details
(define-read-only (get-rental-agreement (agreement-id uint))
  (map-get? rental-agreements { agreement-id: agreement-id })
)

;; Get dispute details
(define-read-only (get-dispute (agreement-id uint))
  (map-get? disputes { agreement-id: agreement-id })
)

;; Get user reputation
(define-read-only (get-user-reputation (user principal))
  (default-to
    {
      total-score: u0,
      review-count: u0,
      as-owner-score: u0,
      as-owner-count: u0,
      as-renter-score: u0,
      as-renter-count: u0
    }
    (map-get? user-reputation { user: user })
  )
)

;; Get average rating for a user
(define-read-only (get-average-rating (user principal))
  (let
    (
      (reputation (get-user-reputation user))
      (review-count (get review-count reputation))
      (total-score (get total-score reputation))
    )
    (if (> review-count u0)
      (/ total-score review-count)
      u0
    )
  )
)

;; Get review details
(define-read-only (get-review (review-id uint))
  (map-get? reviews { review-id: review-id })
)

;; Public functions

;; Create a new property listing
(define-public (create-listing
  (title (string-ascii 100))
  (description (string-utf8 500))
  (location (string-ascii 100))
  (price-per-night uint)
  (security-deposit uint)
  (min-nights uint)
  (max-nights uint)
  (amenities (list 20 (string-ascii 50)))
  (availability-start uint)
  (availability-end uint)
)
  (let
    (
      (listing-id (var-get next-listing-id))
      (current-time (unwrap-panic (get-block-info? time u0)))
    )
    (asserts! (validate-dates availability-start availability-end) ERR-INVALID-DATES)
    (asserts! (> price-per-night u0) ERR-INVALID-LISTING-DATA)
    (asserts! (>= max-nights min-nights) ERR-INVALID-LISTING-DATA)
    (asserts! (> min-nights u0) ERR-INVALID-LISTING-DATA)

    ;; Create the listing
    (map-set property-listings
      { listing-id: listing-id }
      {
        owner: tx-sender,
        title: title,
        description: description,
        location: location,
        price-per-night: price-per-night,
        security-deposit: security-deposit,
        min-nights: min-nights,
        max-nights: max-nights,
        amenities: amenities,
        availability-start: availability-start,
        availability-end: availability-end,
        status: STATUS-ACTIVE,
        created-at: current-time
      }
    )

    ;; Increment the listing ID counter
    (var-set next-listing-id (+ listing-id u1))

    ;; Return the listing ID
    (ok listing-id)
  )
)

;; Update an existing property listing
(define-public (update-listing
  (listing-id uint)
  (title (string-ascii 100))
  (description (string-utf8 500))
  (location (string-ascii 100))
  (price-per-night uint)
  (security-deposit uint)
  (min-nights uint)
  (max-nights uint)
  (amenities (list 20 (string-ascii 50)))
  (availability-start uint)
  (availability-end uint)
  (status uint)
)
  (let
    (
      (listing (unwrap! (map-get? property-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
      (owner (get owner listing))
      (active-agreement (map-get? listing-to-agreement { listing-id: listing-id }))
    )
    ;; Verify ownership
    (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
    ;; Validate new data
    (asserts! (validate-dates availability-start availability-end) ERR-INVALID-DATES)
    (asserts! (> price-per-night u0) ERR-INVALID-LISTING-DATA)
    (asserts! (>= max-nights min-nights) ERR-INVALID-LISTING-DATA)
    (asserts! (> min-nights u0) ERR-INVALID-LISTING-DATA)
    ;; Can't change certain details if there's an active rental
    (asserts! (is-none active-agreement) ERR-RENTAL-AGREEMENT-NOT-FOUND)

    ;; Update the listing
    (map-set property-listings
      { listing-id: listing-id }
      {
        owner: owner,
        title: title,
        description: description,
        location: location,
        price-per-night: price-per-night,
        security-deposit: security-deposit,
        min-nights: min-nights,
        max-nights: max-nights,
        amenities: amenities,
        availability-start: availability-start,
        availability-end: availability-end,
        status: status,
        created-at: (get created-at listing)
      }
    )

    (ok true)
  )
)

;; Deactivate a listing
(define-public (deactivate-listing (listing-id uint))
  (let
    (
      (listing (unwrap! (map-get? property-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
      (owner (get owner listing))
      (active-agreement (map-get? listing-to-agreement { listing-id: listing-id }))
    )
    ;; Verify ownership
    (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
    ;; Can't deactivate if there's an active rental
    (asserts! (is-none active-agreement) ERR-RENTAL-AGREEMENT-NOT-FOUND)

    ;; Update the listing status
    (map-set property-listings
      { listing-id: listing-id }
      (merge listing { status: STATUS-INACTIVE })
    )

    (ok true)
  )
)

;; Submit a rental application
(define-public (apply-for-rental
  (listing-id uint)
  (start-date uint)
  (end-date uint)
  (message (string-utf8 300))
)
  (let
    (
      (listing (unwrap! (map-get? property-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
      (owner (get owner listing))
      (application-id (var-get next-application-id))
      (current-time (unwrap-panic (get-block-info? time u0)))
      (nights (/ (- end-date start-date) u86400))
    )
    ;; Verify listing is active
    (asserts! (is-eq (get status listing) STATUS-ACTIVE) ERR-LISTING-NOT-ACTIVE)
    ;; Verify dates are valid
    (asserts! (validate-dates start-date end-date) ERR-INVALID-DATES)
    ;; Verify dates are within availability window
    (asserts! (and
      (>= start-date (get availability-start listing))
      (<= end-date (get availability-end listing))
    ) ERR-INVALID-DATES)
    ;; Verify nights are within min/max
    (asserts! (and
      (>= nights (get min-nights listing))
      (<= nights (get max-nights listing))
    ) ERR-INVALID-DATES)
    ;; Applicant cannot be the owner
    (asserts! (not (is-eq tx-sender owner)) ERR-NOT-AUTHORIZED)

    ;; Create the application
    (map-set rental-applications
      { application-id: application-id }
      {
        listing-id: listing-id,
        applicant: tx-sender,
        start-date: start-date,
        end-date: end-date,
        message: message,
        status: STATUS-PENDING,
        created-at: current-time
      }
    )

    ;; Increment the application ID counter
    (var-set next-application-id (+ application-id u1))

    (ok application-id)
  )
)

;; Approve a rental application and create a rental agreement
(define-public (approve-application (application-id uint))
  (let
    (
      (application (unwrap! (map-get? rental-applications { application-id: application-id }) ERR-APPLICATION-NOT-FOUND))
      (listing-id (get listing-id application))
      (listing (unwrap! (map-get? property-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
      (owner (get owner listing))
      (applicant (get applicant application))
      (start-date (get start-date application))
      (end-date (get end-date application))
      (total-price (calculate-total-price (get price-per-night listing) start-date end-date))
      (platform-fee (calculate-platform-fee total-price))
      (agreement-id (var-get next-agreement-id))
      (current-time (unwrap-panic (get-block-info? time u0)))
      (active-agreement (map-get? listing-to-agreement { listing-id: listing-id }))
    )
    ;; Verify ownership
    (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
    ;; Verify application is pending
    (asserts! (is-eq (get status application) STATUS-PENDING) ERR-APPLICATION-ALREADY-PROCESSED)
    ;; Verify listing has no active agreement
    (asserts! (is-none active-agreement) ERR-ALREADY-LISTED)

    ;; Update application status
    (map-set rental-applications
      { application-id: application-id }
      (merge application { status: STATUS-APPROVED })
    )

    ;; Create rental agreement
    (map-set rental-agreements
      { agreement-id: agreement-id }
      {
        listing-id: listing-id,
        owner: owner,
        renter: applicant,
        start-date: start-date,
        end-date: end-date,
        total-price: total-price,
        security-deposit: (get security-deposit listing),
        platform-fee: platform-fee,
        status: STATUS-PENDING,
        created-at: current-time
      }
    )

    ;; Link listing to agreement
    (map-set listing-to-agreement
      { listing-id: listing-id }
      { agreement-id: agreement-id }
    )

    ;; Update listing status to inactive (reserved)
    (map-set property-listings
      { listing-id: listing-id }
      (merge listing { status: STATUS-INACTIVE })
    )

    ;; Increment the agreement ID counter
    (var-set next-agreement-id (+ agreement-id u1))

    (ok agreement-id)
  )
)

;; Reject a rental application
(define-public (reject-application (application-id uint))
  (let
    (
      (application (unwrap! (map-get? rental-applications { application-id: application-id }) ERR-APPLICATION-NOT-FOUND))
      (listing-id (get listing-id application))
      (listing (unwrap! (map-get? property-listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
      (owner (get owner listing))
    )
    ;; Verify ownership
    (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
    ;; Verify application is pending
    (asserts! (is-eq (get status application) STATUS-PENDING) ERR-APPLICATION-ALREADY-PROCESSED)

    ;; Update application status
    (map-set rental-applications
      { application-id: application-id }
      (merge application { status: STATUS-REJECTED })
    )

    (ok true)
  )
)

;; Pay security deposit and rent to activate rental agreement
(define-public (pay-rental (agreement-id uint))
  (let
    (
      (agreement (unwrap! (map-get? rental-agreements { agreement-id: agreement-id }) ERR-RENTAL-AGREEMENT-NOT-FOUND))
      (renter (get renter agreement))
      (owner (get owner agreement))
      (total-price (get total-price agreement))
      (security-deposit (get security-deposit agreement))
      (platform-fee (get platform-fee agreement))
      (total-payment (+ total-price security-deposit platform-fee))
    )
    ;; Verify caller is the renter
    (asserts! (is-eq tx-sender renter) ERR-NOT-AUTHORIZED)
    ;; Verify agreement is pending
    (asserts! (is-eq (get status agreement) STATUS-PENDING) ERR-RENTAL-NOT-ACTIVE)

    ;; Transfer payment and security deposit
    (try! (stx-transfer? total-payment tx-sender PLATFORM-ADDRESS))

    ;; Update agreement status
    (map-set rental-agreements
      { agreement-id: agreement-id }
      (merge agreement { status: STATUS-ACTIVE })
    )

    (ok true)
  )
)

;; Complete a rental and release funds
(define-public (complete-rental (agreement-id uint))
  (let
    (
      (agreement (unwrap! (map-get? rental-agreements { agreement-id: agreement-id }) ERR-RENTAL-AGREEMENT-NOT-FOUND))
      (listing-id (get listing-id agreement))
      (renter (get renter agreement))
      (owner (get owner agreement))
      (total-price (get total-price agreement))
      (security-deposit (get security-deposit agreement))
      (platform-fee (get platform-fee agreement))
      (current-time (unwrap-panic (get-block-info? time u0)))
      (end-date (get end-date agreement))
    )
    ;; Only platform can complete rentals for now
    ;; In a more advanced system, this would be automated or have multiple authorization paths
    (asserts! (is-eq tx-sender PLATFORM-ADDRESS) ERR-NOT-AUTHORIZED)
    ;; Verify agreement is active
    (asserts! (is-eq (get status agreement) STATUS-ACTIVE) ERR-RENTAL-NOT-ACTIVE)
    ;; Verify rental period has ended
    (asserts! (>= current-time end-date) ERR-RENTAL-NOT-COMPLETED)
    ;; Verify no active disputes
    (asserts! (is-none (map-get? disputes { agreement-id: agreement-id })) ERR-DISPUTE-EXISTS)

    ;; Transfer rental payment to owner
    (try! (as-contract (stx-transfer? total-price PLATFORM-ADDRESS owner)))
    ;; Transfer security deposit back to renter
    (try! (as-contract (stx-transfer? security-deposit PLATFORM-ADDRESS renter)))
    ;; Platform fee is kept

    ;; Update agreement status
    (map-set rental-agreements
      { agreement-id: agreement-id }
      (merge agreement { status: STATUS-COMPLETED })
    )

    ;; Remove listing-to-agreement mapping
    (map-delete listing-to-agreement { listing-id: listing-id })

    ;; Make listing available again
    (match (map-get? property-listings { listing-id: listing-id })
      listing (map-set property-listings
                { listing-id: listing-id }
                (merge listing { status: STATUS-ACTIVE }))
      false
    )

    (ok true)
  )
)

;; Submit a dispute for a rental
(define-public (create-dispute (agreement-id uint) (reason (string-utf8 500)))
  (let
    (
      (agreement (unwrap! (map-get? rental-agreements { agreement-id: agreement-id }) ERR-RENTAL-AGREEMENT-NOT-FOUND))
      (renter (get renter agreement))
      (owner (get owner agreement))
      (current-time (unwrap-panic (get-block-info? time u0)))
      (existing-dispute (map-get? disputes { agreement-id: agreement-id }))
    )
    ;; Verify caller is either owner or renter
    (asserts! (or (is-eq tx-sender owner) (is-eq tx-sender renter)) ERR-NOT-AUTHORIZED)
    ;; Verify agreement is active
    (asserts! (is-eq (get status agreement) STATUS-ACTIVE) ERR-RENTAL-NOT-ACTIVE)
    ;; Verify no existing dispute
    (asserts! (is-none existing-dispute) ERR-DISPUTE-EXISTS)

    ;; Create dispute
    (map-set disputes
      { agreement-id: agreement-id }
      {
        raised-by: tx-sender,
        reason: reason,
        status: STATUS-PENDING,
        resolution: none,
        created-at: current-time
      }
    )

    ;; Update agreement status
    (map-set rental-agreements
      { agreement-id: agreement-id }
      (merge agreement { status: STATUS-DISPUTED })
    )

    (ok true)
  )
)

;; Resolve a dispute (platform only)
(define-public (resolve-dispute 
  (agreement-id uint) 
  (resolution (string-utf8 500))
  (refund-to-renter uint)  ;; percentage of security deposit to return to renter (0-100)
)
  (let
    (
      (dispute (unwrap! (map-get? disputes { agreement-id: agreement-id }) ERR-DISPUTE-EXISTS))
      (agreement (unwrap! (map-get? rental-agreements { agreement-id: agreement-id }) ERR-RENTAL-AGREEMENT-NOT-FOUND))
      (listing-id (get listing-id agreement))
      (renter (get renter agreement))
      (owner (get owner agreement))
      (total-price (get total-price agreement))
      (security-deposit (get security-deposit agreement))
      (platform-fee (get platform-fee agreement))
      (renter-refund (/ (* security-deposit refund-to-renter) u100))
      (owner-payment (/ (* total-price (- u100 refund-to-renter)) u100))
    )
    ;; Only platform can resolve disputes
    (asserts! (is-eq tx-sender PLATFORM-ADDRESS) ERR-NOT-AUTHORIZED)
    ;; Verify dispute is pending
    (asserts! (is-eq (get status dispute) STATUS-PENDING) ERR-APPLICATION-ALREADY-PROCESSED)
    ;; Verify agreement is disputed
    (asserts! (is-eq (get status agreement) STATUS_DISPUTED) ERR-RENTAL-NOT-ACTIVE)
    ;; Verify refund percentage is valid
    (asserts! (<= refund-to-renter u100) ERR-INVALID-PAYMENT)

    ;; Transfer appropriate amounts
    (try! (as-contract (stx-transfer? owner-payment PLATFORM-ADDRESS owner)))
    (try! (as-contract (stx-transfer? renter-refund PLATFORM-ADDRESS renter)))

    ;; Update dispute status
    (map-set disputes
      { agreement-id: agreement-id }
      (merge dispute { 
        status: STATUS-COMPLETED, 
        resolution: (some resolution) 
      })
    )

    ;; Update agreement status
    (map-set rental-agreements
      { agreement-id: agreement-id }
      (merge agreement { status: STATUS-COMPLETED })
    )

    ;; Remove listing-to-agreement mapping
    (map-delete listing-to-agreement { listing-id: listing-id })

    ;; Make listing available again
    (match (map-get? property-listings { listing-id: listing-id })
      listing (map-set property-listings
                { listing-id: listing-id }
                (merge listing { status: STATUS-ACTIVE }))
      false
    )

    (ok true)
  )
)

;; Leave a review
(define-public (leave-review 
  (agreement-id uint) 
  (reviewee principal) 
  (score uint)
  (comment (string-utf8 300))
)
  (let
    (
      (agreement (unwrap! (map-get? rental-agreements { agreement-id: agreement-id }) ERR-RENTAL-AGREEMENT-NOT-FOUND))
      (renter (get renter agreement))
      (owner (get owner agreement))
      (review-id (var-get next-review-id))
      (reviewer-type (if (is-eq tx-sender owner) "owner" "renter"))
      (current-time (unwrap-panic (get-block-info? time u0)))
      (existing-review (map-get? agreement-reviews { agreement-id: agreement-id, reviewer: tx-sender }))
    )
    ;; Verify caller is either owner or renter
    (asserts! (or (is-eq tx-sender owner) (is-eq tx-sender renter)) ERR-NOT-AUTHORIZED)
    ;; Verify reviewee is the other party
    (asserts! (and
      (not (is-eq tx-sender reviewee))
      (or (is-eq reviewee owner) (is-eq reviewee renter))
    ) ERR-NOT-PARTICIPANT)
    ;; Verify agreement is completed
    (asserts! (is-eq (get status agreement) STATUS-COMPLETED) ERR-RENTAL-NOT-COMPLETED)
    ;; Verify score is between 1 and MAX-REVIEW-SCORE
    (asserts! (and (>= score u1) (<= score MAX-REVIEW-SCORE)) ERR-INVALID-REVIEW-SCORE)
    ;; Verify reviewer hasn't already left a review
    (asserts! (is-none existing-review) ERR-ALREADY-LISTED)

    ;; Create review
    (map-set reviews
      { review-id: review-id }
      {
        agreement-id: agreement-id,
        reviewer: tx-sender,
        reviewee: reviewee,
        score: score,
        comment: comment,
        reviewer-type: reviewer-type,
        created-at: current-time
      }
    )

    ;; Track that this user left a review for this agreement
    (map-set agreement-reviews
      { agreement-id: agreement-id, reviewer: tx-sender }
      { review-id: review-id }
    )

    ;; Update reputation
    (update-reputation 
      reviewee 