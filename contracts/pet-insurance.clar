;; Pet Insurance Management Contract
;; A comprehensive veterinary coverage platform with provider networks,
;; claim submission, and treatment pre-authorization for pet healthcare.

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-POLICY (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-CLAIM-NOT-FOUND (err u103))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u104))
(define-constant ERR-PROVIDER-NOT-AUTHORIZED (err u105))
(define-constant ERR-PRE-AUTH-EXPIRED (err u106))
(define-constant ERR-INVALID-AMOUNT (err u107))

;; Data Variables
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var next-preauth-id uint u1)

;; Data Maps
(define-map policies uint {
    owner: principal,
    pet-name: (string-ascii 50),
    pet-type: (string-ascii 20),
    monthly-premium: uint,
    coverage-limit: uint,
    deductible: uint,
    active: bool,
    created-at: uint
})

(define-map policy-balances uint uint)

(define-map authorized-providers principal {
    name: (string-ascii 100),
    speciality: (string-ascii 50),
    active: bool,
    approved-at: uint
})

(define-map claims uint {
    policy-id: uint,
    provider: principal,
    treatment-type: (string-ascii 100),
    amount: uint,
    status: (string-ascii 20),
    submitted-at: uint,
    processed-at: (optional uint),
    preauth-id: (optional uint)
})

(define-map pre-authorizations uint {
    policy-id: uint,
    provider: principal,
    treatment-type: (string-ascii 100),
    estimated-cost: uint,
    approved: bool,
    expires-at: uint,
    created-at: uint
})

;; Public Functions

;; Create Insurance Policy
(define-public (create-policy 
    (pet-name (string-ascii 50))
    (pet-type (string-ascii 20))
    (monthly-premium uint)
    (coverage-limit uint)
    (deductible uint))
    (let ((policy-id (var-get next-policy-id)))
        (asserts! (> monthly-premium u0) ERR-INVALID-AMOUNT)
        (asserts! (> coverage-limit u0) ERR-INVALID-AMOUNT)
        (map-set policies policy-id {
            owner: tx-sender,
            pet-name: pet-name,
            pet-type: pet-type,
            monthly-premium: monthly-premium,
            coverage-limit: coverage-limit,
            deductible: deductible,
            active: true,
            created-at: stacks-block-height
        })
        (map-set policy-balances policy-id u0)
        (var-set next-policy-id (+ policy-id u1))
        (ok policy-id)))

;; Pay Premium
(define-public (pay-premium (policy-id uint) (amount uint))
    (let ((policy (unwrap! (map-get? policies policy-id) ERR-INVALID-POLICY))
          (current-balance (default-to u0 (map-get? policy-balances policy-id))))
        (asserts! (is-eq (get owner policy) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get active policy) ERR-INVALID-POLICY)
        (asserts! (>= amount (get monthly-premium policy)) ERR-INVALID-AMOUNT)
        (map-set policy-balances policy-id (+ current-balance amount))
        (ok true)))

;; Authorize Provider
(define-public (authorize-provider 
    (provider principal)
    (name (string-ascii 100))
    (speciality (string-ascii 50)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set authorized-providers provider {
            name: name,
            speciality: speciality,
            active: true,
            approved-at: stacks-block-height
        })
        (ok true)))

;; Request Pre-authorization
(define-public (request-preauth
    (policy-id uint)
    (treatment-type (string-ascii 100))
    (estimated-cost uint))
    (let ((policy (unwrap! (map-get? policies policy-id) ERR-INVALID-POLICY))
          (provider-info (unwrap! (map-get? authorized-providers tx-sender) ERR-PROVIDER-NOT-AUTHORIZED))
          (preauth-id (var-get next-preauth-id)))
        (asserts! (get active policy) ERR-INVALID-POLICY)
        (asserts! (get active provider-info) ERR-PROVIDER-NOT-AUTHORIZED)
        (asserts! (> estimated-cost u0) ERR-INVALID-AMOUNT)
        (map-set pre-authorizations preauth-id {
            policy-id: policy-id,
            provider: tx-sender,
            treatment-type: treatment-type,
            estimated-cost: estimated-cost,
            approved: false,
            expires-at: (+ stacks-block-height u144), ;; ~24 hours
            created-at: stacks-block-height
        })
        (var-set next-preauth-id (+ preauth-id u1))
        (ok preauth-id)))

;; Approve Pre-authorization
(define-public (approve-preauth (preauth-id uint))
    (let ((preauth (unwrap! (map-get? pre-authorizations preauth-id) ERR-CLAIM-NOT-FOUND)))
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (< stacks-block-height (get expires-at preauth)) ERR-PRE-AUTH-EXPIRED)
        (map-set pre-authorizations preauth-id 
            (merge preauth { approved: true }))
        (ok true)))

;; Submit Claim
(define-public (submit-claim
    (policy-id uint)
    (treatment-type (string-ascii 100))
    (amount uint)
    (preauth-id (optional uint)))
    (let ((policy (unwrap! (map-get? policies policy-id) ERR-INVALID-POLICY))
          (provider-info (unwrap! (map-get? authorized-providers tx-sender) ERR-PROVIDER-NOT-AUTHORIZED))
          (claim-id (var-get next-claim-id)))
        (asserts! (get active policy) ERR-INVALID-POLICY)
        (asserts! (get active provider-info) ERR-PROVIDER-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        ;; Validate pre-auth if provided
        (match preauth-id
            some-preauth-id (let ((preauth (unwrap! (map-get? pre-authorizations some-preauth-id) ERR-CLAIM-NOT-FOUND)))
                (asserts! (get approved preauth) ERR-NOT-AUTHORIZED)
                (asserts! (< stacks-block-height (get expires-at preauth)) ERR-PRE-AUTH-EXPIRED)
                (asserts! (is-eq (get policy-id preauth) policy-id) ERR-INVALID-POLICY))
            true)
        (map-set claims claim-id {
            policy-id: policy-id,
            provider: tx-sender,
            treatment-type: treatment-type,
            amount: amount,
            status: "pending",
            submitted-at: stacks-block-height,
            processed-at: none,
            preauth-id: preauth-id
        })
        (var-set next-claim-id (+ claim-id u1))
        (ok claim-id)))

;; Process Claim
(define-public (process-claim (claim-id uint) (approved bool))
    (let ((claim (unwrap! (map-get? claims claim-id) ERR-CLAIM-NOT-FOUND))
          (policy (unwrap! (map-get? policies (get policy-id claim)) ERR-INVALID-POLICY))
          (balance (default-to u0 (map-get? policy-balances (get policy-id claim)))))
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
        (if approved
            (let ((payout (if (> (get amount claim) (get deductible policy))
                            (- (get amount claim) (get deductible policy))
                            u0)))
                (asserts! (<= payout (get coverage-limit policy)) ERR-INSUFFICIENT-BALANCE)
                (asserts! (<= payout balance) ERR-INSUFFICIENT-BALANCE)
                (map-set policy-balances (get policy-id claim) (- balance payout))
                (map-set claims claim-id 
                    (merge claim { 
                        status: "approved", 
                        processed-at: (some stacks-block-height) 
                    })))
            (map-set claims claim-id 
                (merge claim { 
                    status: "rejected", 
                    processed-at: (some stacks-block-height) 
                })))
        (ok true)))

;; Read-only Functions

(define-read-only (get-policy (policy-id uint))
    (map-get? policies policy-id))

(define-read-only (get-policy-balance (policy-id uint))
    (default-to u0 (map-get? policy-balances policy-id)))

(define-read-only (get-claim (claim-id uint))
    (map-get? claims claim-id))

(define-read-only (get-preauth (preauth-id uint))
    (map-get? pre-authorizations preauth-id))

(define-read-only (get-provider (provider principal))
    (map-get? authorized-providers provider))

(define-read-only (is-policy-owner (policy-id uint) (user principal))
    (match (map-get? policies policy-id)
        policy (is-eq (get owner policy) user)
        false))

(define-read-only (get-coverage-available (policy-id uint))
    (match (map-get? policies policy-id)
        policy (some {
            balance: (get-policy-balance policy-id),
            coverage-limit: (get coverage-limit policy),
            deductible: (get deductible policy),
            monthly-premium: (get monthly-premium policy)
        })
        none))
