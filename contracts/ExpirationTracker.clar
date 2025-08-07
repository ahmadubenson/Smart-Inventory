(define-constant err-expired-item (err u300))
(define-constant err-invalid-expiration-date (err u301))
(define-constant err-no-expiration-set (err u302))

(define-map item-expiration
    { item-id: uint }
    {
        expiration-date: uint,
        batch-number: (string-ascii 50),
        is-perishable: bool,
        days-warning: uint,
        alert-sent: bool
    }
)

(define-map expiration-alerts
    { alert-id: uint }
    {
        item-id: uint,
        expiration-date: uint,
        alert-type: (string-ascii 20),
        days-until-expiry: uint,
        created-at: uint,
        resolved: bool
    }
)

(define-data-var expiration-alert-counter uint u0)

(define-public (set-item-expiration 
    (item-id uint) 
    (expiration-date uint) 
    (batch-number (string-ascii 50)) 
    (is-perishable bool) 
    (days-warning uint)
)
    (begin
        (asserts! (is-eq tx-sender tx-sender) (err u403))
        (asserts! (> expiration-date stacks-block-height) err-invalid-expiration-date)
        (map-set item-expiration
            { item-id: item-id }
            {
                expiration-date: expiration-date,
                batch-number: batch-number,
                is-perishable: is-perishable,
                days-warning: days-warning,
                alert-sent: false
            }
        )
        (ok true)
    )
)

(define-public (check-and-create-expiration-alerts (item-id uint))
    (let 
        (
            (expiration-data (unwrap! (map-get? item-expiration {item-id: item-id}) err-no-expiration-set))
            (current-block stacks-block-height)
            (expiration-date (get expiration-date expiration-data))
            (warning-period (get days-warning expiration-data))
            (days-until-expiry (- expiration-date current-block))
            (new-alert-id (+ (var-get expiration-alert-counter) u1))
        )
        (asserts! (get is-perishable expiration-data) err-no-expiration-set)
        (asserts! (not (get alert-sent expiration-data)) (err u405))
        (asserts! (<= days-until-expiry warning-period) err-invalid-expiration-date)
        
        (map-set expiration-alerts
            { alert-id: new-alert-id }
            {
                item-id: item-id,
                expiration-date: expiration-date,
                alert-type: (if (<= days-until-expiry u0) "expired" "expiring"),
                days-until-expiry: days-until-expiry,
                created-at: current-block,
                resolved: false
            }
        )
        
        (map-set item-expiration
            { item-id: item-id }
            (merge expiration-data { alert-sent: true })
        )
        
        (var-set expiration-alert-counter new-alert-id)
        (ok new-alert-id)
    )
)

(define-public (batch-check-expirations (item-list (list 50 uint)))
    (let 
        (
            (results (map check-single-item-expiration item-list))
        )
        (ok results)
    )
)

(define-private (check-single-item-expiration (item-id uint))
    (match (map-get? item-expiration {item-id: item-id})
        expiration-data 
        (let 
            (
                (days-until-expiry (- (get expiration-date expiration-data) stacks-block-height))
            )
            (if (and 
                    (get is-perishable expiration-data)
                    (<= days-until-expiry (get days-warning expiration-data))
                    (not (get alert-sent expiration-data))
                )
                (some item-id)
                none
            )
        )
        none
    )
)

(define-public (mark-expired-item-disposed (item-id uint))
    (let 
        (
            (expiration-data (unwrap! (map-get? item-expiration {item-id: item-id}) err-no-expiration-set))
        )
        (asserts! (is-eq tx-sender tx-sender) (err u403))
        (asserts! (<= (get expiration-date expiration-data) stacks-block-height) err-invalid-expiration-date)
        
        (map-set item-expiration
            { item-id: item-id }
            (merge expiration-data { alert-sent: false })
        )
        (ok true)
    )
)

(define-public (resolve-expiration-alert (alert-id uint))
    (let 
        (
            (alert-data (unwrap! (map-get? expiration-alerts {alert-id: alert-id}) (err u404)))
        )
        (asserts! (is-eq tx-sender tx-sender) (err u403))
        (asserts! (not (get resolved alert-data)) (err u405))
        
        (map-set expiration-alerts
            { alert-id: alert-id }
            (merge alert-data { resolved: true })
        )
        (ok true)
    )
)

(define-read-only (get-item-expiration (item-id uint))
    (map-get? item-expiration {item-id: item-id})
)

(define-read-only (get-expiration-alert (alert-id uint))
    (map-get? expiration-alerts {alert-id: alert-id})
)

(define-read-only (is-item-expired (item-id uint))
    (match (map-get? item-expiration {item-id: item-id})
        expiration-data (<= (get expiration-date expiration-data) stacks-block-height)
        false
    )
)

(define-read-only (get-days-until-expiry (item-id uint))
    (match (map-get? item-expiration {item-id: item-id})
        expiration-data (- (get expiration-date expiration-data) stacks-block-height)
        u0
    )
)

(define-read-only (get-expiring-items-count)
    (var-get expiration-alert-counter)
)

(define-read-only (needs-expiration-check (item-id uint))
    (match (map-get? item-expiration {item-id: item-id})
        expiration-data 
        (let 
            (
                (days-until-expiry (- (get expiration-date expiration-data) stacks-block-height))
            )
            (and 
                (get is-perishable expiration-data)
                (<= days-until-expiry (get days-warning expiration-data))
                (not (get alert-sent expiration-data))
            )
        )
        false
    )
)
