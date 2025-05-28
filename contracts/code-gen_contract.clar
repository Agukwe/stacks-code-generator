;; code-gen_contract
;; A robust smart contract for generating code templates and managing code generation
;; processes on the Stacks blockchain.
;; 
;; This contract allows:
;; - Registration and management of code templates
;; - Generation of code from templates with custom parameters
;; - Access control and permissions
;; - Template versioning and history tracking
;; - Payment and subscription options for code generation services

;; ========================================
;; Constants
;; ========================================

;; Platform configuration
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-TEMPLATE-NOT-FOUND (err u1001))
(define-constant ERR-TEMPLATE-EXISTS (err u1002))
(define-constant ERR-INVALID-PARAMS (err u1003))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u1004))
(define-constant ERR-PAYMENT-REQUIRED (err u1005))
(define-constant ERR-VERSION-NOT-FOUND (err u1006))
(define-constant ERR-ACCESS-DENIED (err u1007))

;; Template types
(define-constant TYPE-SMART-CONTRACT u1)
(define-constant TYPE-FRONTEND u2)
(define-constant TYPE-API u3)
(define-constant TYPE-FULL-STACK u4)
(define-constant TYPE-TESTING u5)

;; Access levels
(define-constant ACCESS-PUBLIC u1)
(define-constant ACCESS-PREMIUM u2)
(define-constant ACCESS-PRIVATE u3)

;; ========================================
;; Data Maps and Variables
;; ========================================

;; Track registered templates
(define-map templates 
  { template-id: (string-utf8 36) } 
  {
    name: (string-utf8 100),
    description: (string-utf8 500),
    author: principal,
    template-type: uint,
    access-level: uint,
    base-price: uint,
    created-at: uint,
    latest-version: uint,
    total-uses: uint
  }
)

;; Template version content
(define-map template-versions 
  { template-id: (string-utf8 36), version: uint } 
  {
    content: (string-utf8 10000),
    parameters: (list 20 (string-utf8 30)),
    change-notes: (string-utf8 500),
    created-at: uint
  }
)

;; Template access control
(define-map template-permissions
  { template-id: (string-utf8 36), user: principal }
  {
    can-use: bool,
    can-edit: bool,
    expires-at: uint
  }
)

;; Track user subscriptions
(define-map user-subscriptions
  { user: principal }
  {
    subscription-level: uint,
    expires-at: uint,
    credits-remaining: uint
  }
)

;; History of generated code
(define-map generated-code-history
  { generation-id: (string-utf8 36) }
  {
    template-id: (string-utf8 36),
    template-version: uint,
    user: principal,
    params: (list 20 (string-utf8 100)),
    timestamp: uint,
    paid-amount: uint
  }
)

;; Global counters and settings
(define-data-var total-templates uint u0)
(define-data-var total-generations uint u0)
(define-data-var base-fee uint u1000)              ;; in microSTX
(define-data-var premium-subscription-price uint u100000000) ;; 100 STX
(define-data-var platform-revenue uint u0)

;; ========================================
;; Private Functions
;; ========================================

;; Helper to check if caller is contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

;; Get current block height as timestamp
(define-private (get-current-time)
  block-height
)

;; Check if user has access to a template
(define-private (has-template-access (template-id (string-utf8 36)) (user principal))
  (let (
    (template-info (unwrap! (map-get? templates { template-id: template-id }) false))
    (access-level (get access-level template-info))
    (template-author (get author template-info))
    (user-permission (map-get? template-permissions { template-id: template-id, user: user }))
    (user-subscription (map-get? user-subscriptions { user: user }))
    (is-author (is-eq template-author user))
    (is-owner (is-eq user CONTRACT_OWNER))
  )
    (or
      is-author
      is-owner
      (and 
        (is-some user-permission)
        (get can-use (unwrap-panic user-permission))
        (> (get expires-at (unwrap-panic user-permission)) (get-current-time))
      )
      (and
        (is-eq access-level ACCESS-PUBLIC)
        true
      )
      (and
        (is-eq access-level ACCESS-PREMIUM)
        (is-some user-subscription)
        (>= (get subscription-level (unwrap-panic user-subscription)) u1)
        (> (get expires-at (unwrap-panic user-subscription)) (get-current-time))
      )
    )
  )
)

;; Calculate fee for template usage
(define-private (calculate-fee (template-id (string-utf8 36)))
  (let (
    (template-info (map-get? templates { template-id: template-id }))
  )
    (match template-info
      info (let ((base-price (get base-price info)))
             (if (> base-price u0)
               base-price
               (var-get base-fee)))
      (var-get base-fee)
    )
  )
)

;; Generate a simple ID string based on block height
(define-private (generate-id (seed uint))
  (let (
    (time-component (get-current-time))
    (combined-seed (+ time-component seed))
  )
    ;; Simple ID using block height - return as UTF-8 string
    u"generated-code-id"
  )
)

;; Process template with parameters (simplified for demo)
(define-private (process-template (content (string-utf8 10000)) (params (list 20 (string-utf8 100))))
  ;; In a real implementation, this would have logic to replace placeholders
  ;; For this example, we just return the content
  content
)

;; Consume subscription credits
(define-private (consume-credit (user principal))
  (match (map-get? user-subscriptions { user: user })
    subscription
      (let (
        (current-credits (get credits-remaining subscription))
      )
        (if (> current-credits u0)
          (begin
            (map-set user-subscriptions
              { user: user }
              {
                subscription-level: (get subscription-level subscription),
                expires-at: (get expires-at subscription),
                credits-remaining: (- current-credits u1)
              }
            )
            true
          )
          false
        )
      )
    false
  )
)

;; ========================================
;; Public Functions
;; ========================================

;; Register a new template
(define-public (register-template 
  (template-id (string-utf8 36))
  (name (string-utf8 100))
  (description (string-utf8 500))
  (content (string-utf8 10000))
  (parameters (list 20 (string-utf8 30)))
  (template-type uint)
  (access-level uint)
  (base-price uint)
)
  (let (
    (exists (map-get? templates { template-id: template-id }))
    (current-time (get-current-time))
  )
    (asserts! (is-none exists) ERR-TEMPLATE-EXISTS)
    (asserts! (or (is-contract-owner) (is-eq access-level ACCESS-PUBLIC)) ERR-NOT-AUTHORIZED)
    
    ;; Store template metadata
    (map-set templates
      { template-id: template-id }
      {
        name: name,
        description: description,
        author: tx-sender,
        template-type: template-type,
        access-level: access-level,
        base-price: base-price,
        created-at: current-time,
        latest-version: u1,
        total-uses: u0
      }
    )
    
    ;; Store initial version content
    (map-set template-versions
      { template-id: template-id, version: u1 }
      {
        content: content,
        parameters: parameters,
        change-notes: u"Initial version",
        created-at: current-time
      }
    )
    
    ;; Update counter
    (var-set total-templates (+ (var-get total-templates) u1))
    
    (ok template-id)
  )
)

;; Update template metadata
(define-public (update-template-metadata
  (template-id (string-utf8 36))
  (name (string-utf8 100))
  (description (string-utf8 500))
  (template-type uint)
  (access-level uint)
  (base-price uint)
)
  (match (map-get? templates { template-id: template-id })
    template-info
      (begin
        ;; Check authorization
        (asserts! (or
          (is-eq (get author template-info) tx-sender)
          (is-contract-owner)
        ) ERR-NOT-AUTHORIZED)
        
        ;; Update template info
        (map-set templates
          { template-id: template-id }
          {
            name: name,
            description: description,
            author: (get author template-info),
            template-type: template-type,
            access-level: access-level,
            base-price: base-price,
            created-at: (get created-at template-info),
            latest-version: (get latest-version template-info),
            total-uses: (get total-uses template-info)
          }
        )
        (ok true)
      )
    ERR-TEMPLATE-NOT-FOUND
  )
)

;; Add a new version to an existing template
(define-public (add-template-version
  (template-id (string-utf8 36))
  (content (string-utf8 10000))
  (parameters (list 20 (string-utf8 30)))
  (change-notes (string-utf8 500))
)
  (match (map-get? templates { template-id: template-id })
    template_info
      (let (
        (current-version (get latest-version template_info))
        (new-version (+ current-version u1))
        (current-time (get-current-time))
      )
        ;; Check authorization
        (asserts! (or
          (is-eq (get author template_info) tx-sender)
          (is-contract-owner)
        ) ERR-NOT-AUTHORIZED)
        
        ;; Store new version
        (map-set template-versions
          { template-id: template-id, version: new-version }
          {
            content: content,
            parameters: parameters,
            change-notes: change-notes,
            created-at: current-time
          }
        )
        
        ;; Update latest version reference
        (map-set templates
          { template-id: template-id }
          (merge template_info { latest-version: new-version })
        )
        
        (ok new-version)
      )
    ERR-TEMPLATE-NOT-FOUND
  )
)

;; Grant specific permission to user for template
(define-public (grant-template-permission 
  (template-id (string-utf8 36)) 
  (user principal) 
  (can-use bool) 
  (can-edit bool)
  (duration uint)
)
  (match (map-get? templates { template-id: template-id })
    template_info
      (begin
        ;; Check authorization
        (asserts! (or
          (is-eq (get author template_info) tx-sender)
          (is-contract-owner)
        ) ERR-NOT-AUTHORIZED)
        
        ;; Set permission
        (map-set template-permissions
          { template-id: template-id, user: user }
          {
            can-use: can-use,
            can-edit: can-edit,
            expires-at: (+ (get-current-time) duration)
          }
        )
        (ok true)
      )
    ERR-TEMPLATE-NOT-FOUND
  )
)

;; Generate code from a template (paid version)
(define-public (generate-code
  (template-id (string-utf8 36))
  (version-opt (optional uint))
  (params (list 20 (string-utf8 100)))
  (payment uint)
)
  (let (
    (template-info (unwrap! (map-get? templates { template-id: template-id }) ERR-TEMPLATE-NOT-FOUND))
    (version (default-to (get latest-version template-info) version-opt))
    (fee (calculate-fee template-id))
    (has-access (has-template-access template-id tx-sender))
    (can-use-credit (consume-credit tx-sender))
    (generation-id (generate-id (var-get total-generations)))
    (current-time (get-current-time))
  )
    ;; Check version exists
    (asserts! (is-some (map-get? template-versions { template-id: template-id, version: version })) ERR-VERSION-NOT-FOUND)
    
    ;; Check access or payment
    (asserts! (or has-access (>= payment fee) can-use-credit) ERR-ACCESS-DENIED)
    
    ;; Process payment if required
    (if (and (not has-access) (not can-use-credit))
      (begin
        (asserts! (>= payment fee) ERR-INSUFFICIENT-PAYMENT)
        (try! (stx-transfer? payment tx-sender CONTRACT_OWNER))
        (var-set platform-revenue (+ (var-get platform-revenue) payment))
      )
      true
    )
    
    ;; Get template content and process
    (match (map-get? template-versions { template-id: template-id, version: version })
      version-data
        (let (
          (processed-content (process-template (get content version-data) params))
          (total-uses (+ (get total-uses template-info) u1))
        )
          ;; Update usage statistics
          (map-set templates
            { template-id: template-id }
            (merge template-info { total-uses: total-uses })
          )
          
          ;; Record generation history
          (map-set generated-code-history
            { generation-id: generation-id }
            {
              template-id: template-id,
              template-version: version,
              user: tx-sender,
              params: params,
              timestamp: current-time,
              paid-amount: payment
            }
          )
          
          ;; Update counter
          (var-set total-generations (+ (var-get total-generations) u1))
          
          (ok processed-content)
        )
      ERR-VERSION-NOT-FOUND
    )
  )
)

;; Purchase a subscription
(define-public (purchase-subscription (level uint) (duration uint))
  (let (
    (price (if (is-eq level u1) 
               (var-get premium-subscription-price) 
               (* (var-get premium-subscription-price) level)))
    (current-time (get-current-time))
    (expiry (+ current-time (* duration u144))) ;; ~1 day in blocks
    (credits (* level duration u5)) ;; 5 credits per day per level
  )
    ;; Must be at least level 1
    (asserts! (> level u0) ERR-INVALID-PARAMS)
    
    ;; Process payment
    (try! (stx-transfer? price tx-sender CONTRACT_OWNER))
    
    ;; Update subscription record
    (map-set user-subscriptions
      { user: tx-sender }
      {
        subscription-level: level,
        expires-at: expiry,
        credits-remaining: credits
      }
    )
    
    ;; Update platform revenue
    (var-set platform-revenue (+ (var-get platform-revenue) price))
    
    (ok expiry)
  )
)

;; Get template info (read-only)
(define-read-only (get-template-info (template-id (string-utf8 36)))
  (map-get? templates { template-id: template-id })
)

;; Get template version content (read-only)
(define-read-only (get-template-version 
  (template-id (string-utf8 36)) 
  (version uint)
)
  (map-get? template-versions { template-id: template-id, version: version })
)

;; Get user subscription info (read-only)
(define-read-only (get-user-subscription (user principal))
  (map-get? user-subscriptions { user: user })
)

;; Get generation history (read-only)
(define-read-only (get-generation-history (generation-id (string-utf8 36)))
  (map-get? generated-code-history { generation-id: generation-id })
)

;; Get total number of templates
(define-read-only (get-stats)
  {
    total-templates: (var-get total-templates),
    total-generations: (var-get total-generations),
    platform-revenue: (var-get platform-revenue),
    base-fee: (var-get base-fee)
  }
)

;; Contract owner only: update platform settings
(define-public (update-platform-settings 
  (new-base-fee uint) 
  (new-subscription-price uint)
)
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (var-set base-fee new-base-fee)
    (var-set premium-subscription-price new-subscription-price)
    (ok true)
  )
)