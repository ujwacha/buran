(define-module (my services i3blocks)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:export (home-i3blocks-service-type))

(define (rgb? value)
  (and (list? value)
       (= (length value) 3)
       (every (lambda (x) (and (integer? x) (>= x 0) (<= x 255)))
              value)))

(define (rgb->string rgblist)
  (if (not (rgb? rgblist)) (error "rgb->string expects proper rbg list"))
  (format #f "#~{~2,'0x~}" rgblist))

(define (block-string config-particulars)
  (append 
   (map (lambda (element)
	  (let ((var (car element))
		(val (cdr element)))
	    (case var
	      ((interval) (cond
			   ((number? val)
			    (format #f "interval=~a\n" val))
			   ((eq? val 'once)
			    "interval=once\n")
			   ((eq? val 'repeat)
			    "interval=repeat\n")
			   (else
			    (error "interval: expected number, 'once, or 'repeat" val))))

	      ((signal) (if
			 (not (number? val))
			 (error "signal expects a number")
			 (string-append "signal="
					(number->string val)
					"\n")))

	      ((command) (if
			  (not (string? val))
			  (error "command expects a string")
			  (string-append "command="
					 val
					 "\n")))
	      
	      ((full_text) (if
			    (not (string? val))
			    (error "full_text expects a string")
			    (string-append "full_text="
					   val
					   "\n")))
	      ((color) (if
			(not (rgb? val))
			(error "color expects proper rgb")
			(string-append "color="
				       (rgb->string val)
				       "\n")))
	      (else ""))))
	config-particulars)
   '("\n\n")))

(define  (string-list-producer configlist)
  (map (lambda (block)
	 (let ((top-lvl (car block))
	       (remaining (cdr block)))
	   (cons (string-append "[" (symbol->string top-lvl) "]\n")
		 (block-string remaining))))
       configlist))


(define (produce-file configlist)
  (let ((all-the-calcs (string-list-producer configlist)))
    ;; (apply string-append
    ;; 	   (map (lambda (x) (apply string-append x)) all-the-calcs))
    (string-concatenate (map string-concatenate all-the-calcs))))



(define (i3blocks-config-files config)
  (list `(".config/i3blocks/config" 
          ,(plain-file "i3blocks-config" (produce-file config)))))

(define home-i3blocks-service-type
  (service-type
   (name 'home-i3blocks)
   (extensions
    (list (service-extension
	   home-files-service-type i3blocks-config-files)))
   (default-value '())
   (description "Generate i3blocks config file.")))
