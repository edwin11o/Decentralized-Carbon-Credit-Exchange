;; Decentralized Carbon Credit Exchange Smart Contract
;; A marketplace for trading verified carbon credits on Stacks blockchain

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-not-authorized (err u105))
(define-constant err-invalid-price (err u106))
(define-constant err-listing-expired (err u107))
(define-constant err-invalid-verification (err u108))

;; Data Variables
(define-data-var next-credit-id uint u1)
(define-data-var next-listing-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% = 250 basis points
(define-data-var total-credits-issued uint u0)
(define-data-var total-credits-retired uint u0)

;; Data Maps
(define-map carbon-credits
  uint
  {
    issuer: principal,
    project-name: (string-ascii 64),
    project-location: (string-ascii 64),
    amount: uint,
    vintage-year: uint,
    verification-standard: (string-ascii 32),
    issuance-date: uint,
    is-retired: bool,
    metadata-uri: (optional (string-ascii 256))
  }
)

(define-map credit-balances
  { owner: principal, credit-id: uint }
  uint
)

(define-map credit-listings
  uint
  {
    seller: principal,
    credit-id: uint,
    amount: uint,
    price-per-credit: uint,
    expiry-block: uint,
    is-active: bool
  }
)

(define-map authorized-verifiers
  principal
  bool
)

(define-map user-profiles
  principal
  {
    total-issued: uint,
    total-purchased: uint,
    total-retired: uint,
    reputation-score: uint
  }
)

(define-map project-registry
  (string-ascii 64)
  {
    owner: principal,
    total-credits: uint,
    verification-status: bool,
    registry-date: uint
  }
)

;; Private Functions
(define-private (is-authorized-verifier (verifier principal))
  (default-to false (map-get? authorized-verifiers verifier))
)

(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000)
)

(define-private (transfer-credits (from principal) (to principal) (credit-id uint) (amount uint))
  (let 
    (
      (sender-balance (default-to u0 (map-get? credit-balances { owner: from, credit-id: credit-id })))
      (receiver-balance (default-to u0 (map-get? credit-balances { owner: to, credit-id: credit-id })))
    )
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (map-set credit-balances { owner: from, credit-id: credit-id } (- sender-balance amount))
    (map-set credit-balances { owner: to, credit-id: credit-id } (+ receiver-balance amount))
    (ok true)
  )
)

;; Public Functions

;; Issue new carbon credits (only authorized verifiers)
(define-public (issue-carbon-credits
  (project-name (string-ascii 64))
  (project-location (string-ascii 64))
  (amount uint)
  (vintage-year uint)
  (verification-standard (string-ascii 32))
  (recipient principal)
  (metadata-uri (optional (string-ascii 256)))
)
  (let 
    (
      (credit-id (var-get next-credit-id))
    )
    (asserts! (is-authorized-verifier tx-sender) err-not-authorized)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= vintage-year u2000) err-invalid-verification)
    
    ;; Create credit record
    (map-set carbon-credits credit-id
      {
        issuer: tx-sender,
        project-name: project-name,
        project-location: project-location,
        amount: amount,
        vintage-year: vintage-year,
        verification-standard: verification-standard,
        issuance-date: block-height,
        is-retired: false,
        metadata-uri: metadata-uri
      }
    )
    
    ;; Assign credits to recipient
    (map-set credit-balances { owner: recipient, credit-id: credit-id } amount)
    
    ;; Update project registry
    (map-set project-registry project-name
      {
        owner: recipient,
        total-credits: amount,
        verification-status: true,
        registry-date: block-height
      }
    )
    
    ;; Update counters
    (var-set next-credit-id (+ credit-id u1))
    (var-set total-credits-issued (+ (var-get total-credits-issued) amount))
    
    ;; Update user profile
    (let 
      (
        (current-profile (default-to 
          { total-issued: u0, total-purchased: u0, total-retired: u0, reputation-score: u100 }
          (map-get? user-profiles recipient)
        ))
      )
      (map-set user-profiles recipient
        (merge current-profile { total-issued: (+ (get total-issued current-profile) amount) })
      )
    )
    
    (ok credit-id)
  )
)

;; Create a listing to sell carbon credits
(define-public (create-listing
  (credit-id uint)
  (amount uint)
  (price-per-credit uint)
  (duration-blocks uint)
)
  (let 
    (
      (listing-id (var-get next-listing-id))
      (current-balance (default-to u0 (map-get? credit-balances { owner: tx-sender, credit-id: credit-id })))
      (credit-info (unwrap! (map-get? carbon-credits credit-id) err-not-found))
    )
    (asserts! (>= current-balance amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> price-per-credit u0) err-invalid-price)
    (asserts! (not (get is-retired credit-info)) err-invalid-verification)
    
    ;; Create listing
    (map-set credit-listings listing-id
      {
        seller: tx-sender,
        credit-id: credit-id,
        amount: amount,
        price-per-credit: price-per-credit,
        expiry-block: (+ block-height duration-blocks),
        is-active: true
      }
    )
    
    (var-set next-listing-id (+ listing-id u1))
    (ok listing-id)
  )
)

;; Purchase carbon credits from a listing
(define-public (purchase-credits (listing-id uint) (amount uint))
  (let 
    (
      (listing (unwrap! (map-get? credit-listings listing-id) err-not-found))
      (total-cost (* amount (get price-per-credit listing)))
      (platform-fee (calculate-platform-fee total-cost))
      (seller-payment (- total-cost platform-fee))
    )
    (asserts! (get is-active listing) err-not-found)
    (asserts! (<= block-height (get expiry-block listing)) err-listing-expired)
    (asserts! (<= amount (get amount listing)) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Transfer STX payment
    (try! (stx-transfer? total-cost tx-sender (get seller listing)))
    
    ;; Transfer platform fee to contract owner
    (try! (stx-transfer? platform-fee (get seller listing) contract-owner))
    
    ;; Transfer credits
    (try! (transfer-credits (get seller listing) tx-sender (get credit-id listing) amount))
    
    ;; Update listing
    (if (is-eq amount (get amount listing))
      ;; Complete purchase - deactivate listing
      (map-set credit-listings listing-id (merge listing { is-active: false, amount: u0 }))
      ;; Partial purchase - reduce amount
      (map-set credit-listings listing-id (merge listing { amount: (- (get amount listing) amount) }))
    )
    
    ;; Update buyer profile
    (let 
      (
        (buyer-profile (default-to 
          { total-issued: u0, total-purchased: u0, total-retired: u0, reputation-score: u100 }
          (map-get? user-profiles tx-sender)
        ))
      )
      (map-set user-profiles tx-sender
        (merge buyer-profile { total-purchased: (+ (get total-purchased buyer-profile) amount) })
      )
    )
    
    (ok true)
  )
)

;; Retire carbon credits (remove from circulation)
(define-public (retire-credits (credit-id uint) (amount uint))
  (let 
    (
      (current-balance (default-to u0 (map-get? credit-balances { owner: tx-sender, credit-id: credit-id })))
      (credit-info (unwrap! (map-get? carbon-credits credit-id) err-not-found))
    )
    (asserts! (>= current-balance amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (get is-retired credit-info)) err-invalid-verification)
    
    ;; Reduce balance
    (map-set credit-balances { owner: tx-sender, credit-id: credit-id } (- current-balance amount))
    
    ;; Update retirement counter
    (var-set total-credits-retired (+ (var-get total-credits-retired) amount))
    
    ;; Update user profile
    (let 
      (
        (user-profile (default-to 
          { total-issued: u0, total-purchased: u0, total-retired: u0, reputation-score: u100 }
          (map-get? user-profiles tx-sender)
        ))
      )
      (map-set user-profiles tx-sender
        (merge user-profile { total-retired: (+ (get total-retired user-profile) amount) })
      )
    )
    
    (ok true)
  )
)

;; Cancel an active listing
(define-public (cancel-listing (listing-id uint))
  (let 
    (
      (listing (unwrap! (map-get? credit-listings listing-id) err-not-found))
    )
    (asserts! (is-eq (get seller listing) tx-sender) err-not-authorized)
    (asserts! (get is-active listing) err-not-found)
    
    ;; Deactivate listing
    (map-set credit-listings listing-id (merge listing { is-active: false }))
    (ok true)
  )
)

;; Admin Functions

;; Add authorized verifier (only contract owner)
(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-verifiers verifier true)
    (ok true)
  )
)

;; Remove authorized verifier (only contract owner)
(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete authorized-verifiers verifier)
    (ok true)
  )
)

;; Update platform fee rate (only contract owner)
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u1000) err-invalid-amount) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

;; Read-only Functions

;; Get carbon credit details
(define-read-only (get-carbon-credit (credit-id uint))
  (map-get? carbon-credits credit-id)
)

;; Get credit balance for owner
(define-read-only (get-credit-balance (owner principal) (credit-id uint))
  (default-to u0 (map-get? credit-balances { owner: owner, credit-id: credit-id }))
)

;; Get listing details
(define-read-only (get-listing (listing-id uint))
  (map-get? credit-listings listing-id)
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles user)
)

;; Get project registry info
(define-read-only (get-project-info (project-name (string-ascii 64)))
  (map-get? project-registry project-name)
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-credits-issued: (var-get total-credits-issued),
    total-credits-retired: (var-get total-credits-retired),
    platform-fee-rate: (var-get platform-fee-rate),
    next-credit-id: (var-get next-credit-id),
    next-listing-id: (var-get next-listing-id)
  }
)

;; Check if address is authorized verifier
(define-read-only (is-verifier (address principal))
  (is-authorized-verifier address)
)

;; Get contract owner
(define-read-only (get-contract-owner)
  contract-owner
)