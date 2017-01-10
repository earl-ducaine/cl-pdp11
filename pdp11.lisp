(defconstant flagn 8)
(defconstant flagz 4)
(defconstant flagv 2)
(defconstant flagc 1)

(defvar pr nil)
(defvar r (vector 0  0  0  0  0  0  0  0)) ; registers
(defvar ksp) ;kernel and user stack pointer
(defvar usp)
(defvar ps) ; processor status
(defvar curpc) ; address of current instruction
(defvar lastpcs '())
(defvar inst) ; current instruction
(defvar memory (make-array (* 128 1024) :initial-element 0)) ; word addressing
(defvar tim1)
(defvar tim2)
(defvar ips)
(defvar sr0)
(defvar sr2)
(defvar curuser)
(defvar prevuser)
(defvar lks)
(defvar clkcounter)
(defvar waiting nil)
(defvar interrupts '())
(defvar pages (make-array 16))

;; traps
(defvar INTBUS 0004)
(defvar INTINVAL 0010)
(defvar INTDEBUG 0014)
(defvar INTIOT 0020)
(defvar INTTTYIN 0060)
(defvar INTTTYOUT 0064)
(defvar INTFAULT 0250)
(defvar INTCLOCK 0100)
(defvar INTRK 0220)

(defvar bootrom
  '(0042113               ; "KD"
    0012706 02000         ; MOV #boot_start, SP
    0012700 0000000       ; MOV #unit, R0        ; unit number
    0010003               ; MOV R0, R3
    0000303               ; SWAB R3
    0006303               ; ASL R3
    0006303               ; ASL R3
    0006303               ; ASL R3
    0006303               ; ASL R3
    0006303               ; ASL R3
    0012701 0177412       ; MOV #RKDA, R1        ; csr
    0010311               ; MOV R3, (R1)         ; load da
    0005041               ; CLR -(R1)            ; clear ba
    0012741 0177000       ; MOV #-256.*2, -(R1)  ; load wc
    0012741 0000005       ; MOV #READ+GO, -(R1)  ; read & go
    0005002               ; CLR R2
    0005003               ; CLR R3
    0012704 02020         ; MOV #START+20, R4
    0005005               ; CLR R5
    0105711               ; TSTB (R1)
    0100376               ; BPL .-2
    0105011               ; CLRB (R1)
    0005007               ; CLR PC
    ))

;; Note, unlike builtins 'or' 'and', xor always evaluates both
;; arguments and returns either nil or t, never the values of the
;; arguments.
(defun xor (a b)
  (and (or a b) (not (and a b))))

(defun switchmode (newm)
  (setf prevuser curuser)
  (setf curuser newm)
  (if prevuser
      (setf usp (aref r 6))
      (setf ksp (aref r 6)))
  (setf (aref r 6)
	(if curuser
	    usp ksp))
  (setf ps (mask-field (byte 32 0) ps))
  (when curuser
    ;; set 14th and 15th bit.
    (setf ps (dpb #b11 (byte 2 14) ps)))
  (when prevuser
    ;; set 12th and 13th bit.
    (setf ps (dpb #b11 (byte 2 12) ps))))

(defun  physread16 (a)
  (cond
    ((a & 1)
     (throw (trap INTBUS, (concatenate 'string "read from odd address " (ostr a 6)))))
    ((< a #o0760000)
     (return (aref memory (ash a -1))))
    ((= a #o777546)
     (return LKS))
    ((= a #o777570)
     (return #o173030))
    ((a == #o777572)
     (return SR0))
    ((a == #o777576)
     (return SR2))
    ((a == #o777776)
     (return PS))
    ((a & #o777770) == #o777560
     (return consread16(a)))
    ((a & #o777760) == #o777400
     (return rkread16(a)))
    ((a & #o777600) == #o772200 || (a & #o777600) == #o777600
     (return mmuread16(a)))
    ((a == #o776000)
     panic("lolwut")))
  (throw Trap(INTBUS, "read from invalid address " + ostr(a,6))))


(defun ostr (z n)
  (let ((val (format nil "~o" z)))
    (unless n
      (setf n 6))
    while(val.length < n)
	val = "0"+val;
    return val;
}
