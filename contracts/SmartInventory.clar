;; SmartInventory
;; Supply chain inventory management system

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-invalid-quantity (err u101))
(define-constant err-item-exists (err u102))
(define-constant err-item-not-found (err u103))
(define-constant err-insufficient-stock (err u104))
(define-constant err-invalid-supplier (err u105))

;; Data Variables
(define-data-var total-items uint u0)
(define-data-var total-suppliers uint u0)

;; Data Maps
(define-map inventory
    { item-id: uint }
    {
        name: (string-ascii 50),
        quantity: uint,
        min-threshold: uint,
        supplier-id: uint,
        price: uint,
        last-updated: uint
    }
)

(define-map suppliers
    { supplier-id: uint }
    {
        name: (string-ascii 50),
        verified: bool,
        rating: uint,
        active: bool
    }
)

(define-map orders
    { order-id: uint }
    {
        item-id: uint,
        quantity: uint,
        status: (string-ascii 20),
        timestamp: uint
    }
)

;; Public Functions

;; Add new inventory item
(define-public (add-item (name (string-ascii 50)) (quantity uint) (min-threshold uint) (supplier-id uint) (price uint))
    (let
        ((new-item-id (+ (var-get total-items) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-supplier-valid supplier-id) err-invalid-supplier)
        (asserts! (> quantity u0) err-invalid-quantity)
        (asserts! (map-insert inventory
            { item-id: new-item-id }
            {
                name: name,
                quantity: quantity,
                min-threshold: min-threshold,
                supplier-id: supplier-id,
                price: price,
                last-updated: stacks-block-height
            }
        ) err-item-exists)
        (var-set total-items new-item-id)
        (ok new-item-id)
    )
)

;; Update inventory quantity
(define-public (update-quantity (item-id uint) (new-quantity uint))
    (let ((item (unwrap! (map-get? inventory {item-id: item-id}) err-item-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (>= new-quantity u0) err-invalid-quantity)
        (map-set inventory
            {item-id: item-id}
            (merge item {
                quantity: new-quantity,
                last-updated: stacks-block-height
            })
        )
        (ok true)
    )
)

;; Add new supplier
(define-public (add-supplier (name (string-ascii 50)))
    (let
        ((new-supplier-id (+ (var-get total-suppliers) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (map-insert suppliers
            { supplier-id: new-supplier-id }
            {
                name: name,
                verified: false,
                rating: u0,
                active: true
            }
        ) err-item-exists)
        (var-set total-suppliers new-supplier-id)
        (ok new-supplier-id)
    )
)

;; Create order
(define-public (create-order (item-id uint) (quantity uint))
    (let
        ((item (unwrap! (map-get? inventory {item-id: item-id}) err-item-not-found)))
        (asserts! (>= (get quantity item) quantity) err-insufficient-stock)
        (asserts! (map-insert orders
            { order-id: (+ stacks-block-height u1) }
            {
                item-id: item-id,
                quantity: quantity,
                status: "pending",
                timestamp: stacks-block-height
            }
        ) err-item-exists)
        (try! (update-quantity item-id (- (get quantity item) quantity)))
        (ok true)
    )
)

;; Read Only Functions

;; Check if supplier exists and is active
(define-read-only (is-supplier-valid (supplier-id uint))
    (match (map-get? suppliers {supplier-id: supplier-id})
        supplier (get active supplier)
        false
    )
)

;; Get item details
(define-read-only (get-item (item-id uint))
    (map-get? inventory {item-id: item-id})
)

;; Get supplier details
(define-read-only (get-supplier (supplier-id uint))
    (map-get? suppliers {supplier-id: supplier-id})
)

;; Get order details
(define-read-only (get-order (order-id uint))
    (map-get? orders {order-id: order-id})
)

;; Check if item needs restock
(define-read-only (needs-restock (item-id uint))
    (match (map-get? inventory {item-id: item-id})
        item (< (get quantity item) (get min-threshold item))
        false
    )
)

