#lang racket
(require (rename-in racket [date? rb:date?] [date rb:date]))
(require gregor racket/match pict)
(provide draw-year all-years get-year create-date create-pdf key)

(struct key (name start end color))
;Helper Functions ----------------------------------------------------------------------------------
;Predicate that determines if a number or a date is between a certian value. Dispatches differntly on Date and Number types
(define (between? x start end)
  (cond [(andmap number? `(,x ,start ,end))
         (and (>= x start) (<= x end))]
        [(andmap date? `(,x ,start ,end))
         (and (date>=? x start) (date<=? x end))]))

(define month-strings #("January" "February" "March" "April"
                            "May" "June" "July" "August"
                            "September" "October" "November" "December"))
(define (month-string month)
    (vector-ref month-strings (- month 1)))

; Makes creating rectangle picts more concise
(define (make-rectangle w h color border-color border-width)
    (filled-rectangle w h #:color color #:border-color border-color #:border-width border-width))

(define (create-pdf pdf% year-picts name w h)
  (define dc
    (new pdf%
         [interactive #f]
         [use-paper-bbox #f]
         [width w]
         [height h]
         [as-eps #f]
         [output (string-append name ".pdf")]))
  (define-values (width height) (send dc get-size))
  
  ;Draws each year on to a seperate page of the pdf
  (send dc start-doc "file output")
  (for ([year year-picts])
    (send dc start-page)
    (send dc draw-bitmap
          (pict->bitmap
           (scale-to-fit year width height))
          0 0)
    (send dc end-page))
  (send dc end-doc))

; Draw Procedures ----------------------------------------------------------------------------------
(define (legend-pict legend)
  (for/fold ([acc (blank 0)])
            ([key legend])
    (let ([color (key-color key)])
      (hc-append acc (make-rectangle 12 12 color color 0) (blank 3) (text (key-name key)) (blank 10)))))

(define (day-picts year month legend)
  ;Marks days on a calender according their range.
  (define (marker day)
    (define mark-list
      (let ([c-date (date year month day)])
        (filter (λ (k) (between? c-date (key-start k) (key-end k))) legend)))
    (define marks-length (length mark-list))

    ;Fixes glitch where months without marks do not render properly due to issues with size
    (when (zero? marks-length)
      (set! mark-list (list (key "" (date 1 1 1) (date 1 1 1) "white")))
      (set! marks-length 1))
    
    (define marks
      (frame
       (for/fold ([acc (blank 0)])
                 ([key mark-list])
         (let ([color (key-color key)])
           (vc-append acc (make-rectangle 50 (/ 35 marks-length) color color 0))))
       #:color "black" #:line-width 3))
    
    (cc-superimpose
     marks
     (text (format "~s" day))))
  
  (define num-of-days (days-in-month year month))
  (define start-day (->wday (date year month 1)))
  ;Both args from in-range are subtracted by the start date so that "dead" calender days show up correctly
  (for/list ([day (in-range (- 1 start-day) (- 36 start-day))])
    (if (between? day 1 num-of-days)
        (marker day)
        (make-rectangle 50 35 "gray" "black" 3))))

(define (month-pict month year legend)
  (define month-label-pict
    ;The magic numbers in the arguments for make-rectangle are to set ratios, as the calender is scaled to fit later
    (cc-superimpose (make-rectangle 380 25 "white" "black" 3)
                    (text (month-string month))))

  (define week-label-picts 
    (for/list ([week-day '("Sun" "Mon" "Tues" "Wed" "Thurs" "Fri" "Sat")])
      (cc-superimpose (make-rectangle 50 25 "white" "black" 3)
                      (text week-day))))
 
  (vc-append month-label-pict
             (blank 5)
             (table 7
                    (append week-label-picts
                            (day-picts year month legend))
                    cc-superimpose
                    cc-superimpose
                    5
                    5)))

(define (year-pict year legend)
  (define year-label
    (cc-superimpose (make-rectangle 1550 25 "white" "black" 3)
                    (text (format "~s" year))))
  (define month-labels
    (table 4
           (for/list ([month (in-range 1 13)])
             (month-pict month year legend))
           cc-superimpose
           cc-superimpose
           10
           10))
  
  (vc-append (blank 5)
             (legend-pict legend)
             (blank 7)
             year-label
             (blank 10)
             month-labels))
;Loads every year-pict into a vector for the GUI to use
(define (all-years start end legend w h)
  (define width (* w 9/10))
  (define height (* h 9/10))
  (for/vector ([year (in-range start (add1 end))])
    (cc-superimpose
     (rectangle w h #:border-color "white" #:border-width 0)
     (scale-to-fit (year-pict year legend) width height))))

(define (get-year years index)
  (vector-ref years index))

(define (draw-year pict dc)
  (draw-pict pict dc 0 0))

(define (create-date year month day)
  (date year month day))