;; Fleet Telematics Smart Contract
;; Track vehicle usage, schedule maintenance, and monitor driver performance

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-NOT-AUTHORIZED (err u102))
(define-constant ERR-INVALID-DATA (err u103))
(define-constant ERR-MAINTENANCE-DUE (err u104))

;; Vehicle Status Constants
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-MAINTENANCE u2)
(define-constant STATUS-INACTIVE u3)
(define-constant STATUS-RETIRED u4)

;; Vehicle Type Constants
(define-constant TYPE-SEDAN u1)
(define-constant TYPE-TRUCK u2)
(define-constant TYPE-VAN u3)
(define-constant TYPE-SUV u4)

;; Data Variables
(define-data-var vehicle-counter uint u0)
(define-data-var maintenance-counter uint u0)

;; Data Maps
(define-map vehicles
    { vehicle-id: uint }
    {
        fleet-owner: principal,
        vehicle-type: uint,
        make-model: (string-ascii 64),
        year: uint,
        vin: (string-ascii 32),
        license-plate: (string-ascii 16),
        status: uint,
        registration-date: uint,
        mileage: uint,
        last-service: uint,
        next-service-due: uint,
        insurance-premium: uint,
        driver-assigned: (optional principal)
    }
)

(define-map telematics-data
    { vehicle-id: uint, timestamp: uint }
    {
        location-lat: int,
        location-lon: int,
        speed-kmh: uint,
        fuel-level: uint,
        engine-temp: uint,
        rpm: uint,
        harsh-braking: bool,
        rapid-acceleration: bool,
        idle-time: uint
    }
)

(define-map driver-performance
    { driver: principal, vehicle-id: uint }
    {
        total-miles: uint,
        safety-score: uint,
        fuel-efficiency: uint,
        speeding-incidents: uint,
        harsh-events: uint,
        last-updated: uint
    }
)

(define-map maintenance-records
    { maintenance-id: uint }
    {
        vehicle-id: uint,
        maintenance-type: (string-ascii 64),
        cost: uint,
        service-date: uint,
        next-service: uint,
        technician: principal,
        notes: (string-ascii 256)
    }
)

(define-map authorized-telematics-devices principal bool)
(define-map fleet-managers principal bool)

;; Authorization functions
(define-public (add-telematics-device (device principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (ok (map-set authorized-telematics-devices device true))
    )
)

(define-public (add-fleet-manager (manager principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (ok (map-set fleet-managers manager true))
    )
)

;; Helper functions
(define-private (increment-vehicle-counter)
    (let ((current (var-get vehicle-counter)))
        (var-set vehicle-counter (+ current u1))
        (+ current u1)
    )
)

(define-private (increment-maintenance-counter)
    (let ((current (var-get maintenance-counter)))
        (var-set maintenance-counter (+ current u1))
        (+ current u1)
    )
)

;; Vehicle Registration
(define-public (register-vehicle
    (vehicle-type uint)
    (make-model (string-ascii 64))
    (year uint)
    (vin (string-ascii 32))
    (license-plate (string-ascii 16)))
    (let ((vehicle-id (increment-vehicle-counter)))
        (asserts! (<= vehicle-type TYPE-SUV) ERR-INVALID-DATA)
        (asserts! (> year u1990) ERR-INVALID-DATA)
        
        (map-set vehicles
            { vehicle-id: vehicle-id }
            {
                fleet-owner: tx-sender,
                vehicle-type: vehicle-type,
                make-model: make-model,
                year: year,
                vin: vin,
                license-plate: license-plate,
                status: STATUS-ACTIVE,
                registration-date: stacks-block-height,
                mileage: u0,
                last-service: stacks-block-height,
                next-service-due: (+ stacks-block-height u5000),
                insurance-premium: u100,
                driver-assigned: none
            }
        )
        (ok vehicle-id)
    )
)

;; Telematics Data Submission
(define-public (submit-telematics-data
    (vehicle-id uint)
    (location-lat int)
    (location-lon int)
    (speed-kmh uint)
    (fuel-level uint)
    (engine-temp uint)
    (rpm uint)
    (harsh-braking bool)
    (rapid-acceleration bool)
    (idle-time uint))
    (let ((vehicle (unwrap! (map-get? vehicles { vehicle-id: vehicle-id }) ERR-NOT-FOUND)))
        (asserts! (default-to false (map-get? authorized-telematics-devices tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status vehicle) STATUS-ACTIVE) ERR-INVALID-DATA)
        
        (map-set telematics-data
            { vehicle-id: vehicle-id, timestamp: stacks-block-height }
            {
                location-lat: location-lat,
                location-lon: location-lon,
                speed-kmh: speed-kmh,
                fuel-level: fuel-level,
                engine-temp: engine-temp,
                rpm: rpm,
                harsh-braking: harsh-braking,
                rapid-acceleration: rapid-acceleration,
                idle-time: idle-time
            }
        )
        
        ;; Update vehicle mileage (simplified)
        (map-set vehicles
            { vehicle-id: vehicle-id }
            (merge vehicle { mileage: (+ (get mileage vehicle) u1) })
        )
        (ok true)
    )
)

;; Driver Assignment
(define-public (assign-driver (vehicle-id uint) (driver principal))
    (let ((vehicle (unwrap! (map-get? vehicles { vehicle-id: vehicle-id }) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get fleet-owner vehicle)) ERR-NOT-AUTHORIZED)
        
        (map-set vehicles
            { vehicle-id: vehicle-id }
            (merge vehicle { driver-assigned: (some driver) })
        )
        
        ;; Initialize driver performance record
        (map-set driver-performance
            { driver: driver, vehicle-id: vehicle-id }
            {
                total-miles: u0,
                safety-score: u100,
                fuel-efficiency: u100,
                speeding-incidents: u0,
                harsh-events: u0,
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Update Driver Performance
(define-public (update-driver-performance
    (vehicle-id uint)
    (driver principal)
    (miles-driven uint)
    (safety-events uint)
    (fuel-efficiency uint))
    (let ((performance (unwrap! (map-get? driver-performance { driver: driver, vehicle-id: vehicle-id }) ERR-NOT-FOUND)))
        (asserts! (default-to false (map-get? authorized-telematics-devices tx-sender)) ERR-NOT-AUTHORIZED)
        
        (let ((new-total-miles (+ (get total-miles performance) miles-driven))
              (new-harsh-events (+ (get harsh-events performance) safety-events))
              (new-safety-score (if (> new-harsh-events u0)
                  (if (> (get safety-score performance) u10) (- (get safety-score performance) u10) u0)
                  (if (< (get safety-score performance) u100) (+ (get safety-score performance) u1) u100)
              )))
            
            (map-set driver-performance
                { driver: driver, vehicle-id: vehicle-id }
                (merge performance {
                    total-miles: new-total-miles,
                    safety-score: new-safety-score,
                    fuel-efficiency: fuel-efficiency,
                    harsh-events: new-harsh-events,
                    last-updated: stacks-block-height
                })
            )
            (ok true)
        )
    )
)

;; Maintenance Scheduling
(define-public (schedule-maintenance
    (vehicle-id uint)
    (maintenance-type (string-ascii 64))
    (estimated-cost uint))
    (let ((vehicle (unwrap! (map-get? vehicles { vehicle-id: vehicle-id }) ERR-NOT-FOUND)))
        (asserts! (default-to false (map-get? fleet-managers tx-sender)) ERR-NOT-AUTHORIZED)
        
        (map-set vehicles
            { vehicle-id: vehicle-id }
            (merge vehicle { status: STATUS-MAINTENANCE })
        )
        
        (let ((maintenance-id (increment-maintenance-counter)))
            (map-set maintenance-records
                { maintenance-id: maintenance-id }
                {
                    vehicle-id: vehicle-id,
                    maintenance-type: maintenance-type,
                    cost: estimated-cost,
                    service-date: stacks-block-height,
                    next-service: (+ stacks-block-height u5000),
                    technician: tx-sender,
                    notes: "Scheduled maintenance"
                }
            )
            (ok maintenance-id)
        )
    )
)

;; Complete Maintenance
(define-public (complete-maintenance
    (maintenance-id uint)
    (actual-cost uint)
    (notes (string-ascii 256)))
    (let ((maintenance (unwrap! (map-get? maintenance-records { maintenance-id: maintenance-id }) ERR-NOT-FOUND))
          (vehicle (unwrap! (map-get? vehicles { vehicle-id: (get vehicle-id maintenance) }) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get technician maintenance)) ERR-NOT-AUTHORIZED)
        
        (map-set maintenance-records
            { maintenance-id: maintenance-id }
            (merge maintenance {
                cost: actual-cost,
                notes: notes
            })
        )
        
        (map-set vehicles
            { vehicle-id: (get vehicle-id maintenance) }
            (merge vehicle {
                status: STATUS-ACTIVE,
                last-service: stacks-block-height,
                next-service-due: (get next-service maintenance)
            })
        )
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-vehicle (vehicle-id uint))
    (map-get? vehicles { vehicle-id: vehicle-id })
)

(define-read-only (get-telematics-data (vehicle-id uint) (timestamp uint))
    (map-get? telematics-data { vehicle-id: vehicle-id, timestamp: timestamp })
)

(define-read-only (get-driver-performance (driver principal) (vehicle-id uint))
    (map-get? driver-performance { driver: driver, vehicle-id: vehicle-id })
)

(define-read-only (get-maintenance-record (maintenance-id uint))
    (map-get? maintenance-records { maintenance-id: maintenance-id })
)

(define-read-only (is-maintenance-due (vehicle-id uint))
    (match (map-get? vehicles { vehicle-id: vehicle-id })
        vehicle (<= (get next-service-due vehicle) stacks-block-height)
        false
    )
)

(define-read-only (get-fleet-stats (fleet-owner principal))
    ;; Simplified fleet statistics - would normally require iteration
    { total-vehicles: (var-get vehicle-counter), total-maintenance: (var-get maintenance-counter) }
)

;; title: fleet-telematics
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

