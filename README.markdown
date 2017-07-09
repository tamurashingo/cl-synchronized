# CL-Synchronized - mutex operation (like Java's synchronized) for CommonLisp

CL-Synchronized provides synchronized operation like Java's `synchronized`.

## Usage (with mutex object)

1. create a mutex object `make-synchronized`
1. use `synchronized` with the mutex object

```common-lisp
(defvar *SYNCHRONIZED* (make-synchronized))

(synchronized *SYNCHRONIZED*
  ...)
```

### Example

```common-lisp
(defvar *SYNCHRONIZED* (make-synchronized))
(defvar *val* 0)

(let ((th-array (make-array 10)))
  (loop for index from 0 below 10
        do (setf (aref th-array index)
                 (bt:make-thread
                   (lambda ()
                     (synchronized *SYNCHRONIZED*
                       (let ((v *val*)
                             (*standard-output* #.*standard-output*))
                         (format T "before:~A~%" *val*)
                         (sleep (random 0.5))
                         (setf *val* (1+ v))
                         (format T "after :~A~%" *val*)))))))
  (loop for index from 0 below 10
        do (bt:join-thread (aref th-array index))))

>before:0
>after :1
>before:1
>after :2
>before:2
>after :3
>before:3
>after :4
>before:4
>after :5
>before:5
>after :6
>before:6
>after :7
>before:7
>after :8
>before:8
>after :9
>before:9
>after :10

(format T "*val* is ~A~%" *val*)

>*val* is 10
```

## Usage (synchronized function)

1. define function with `@synchronized-with` annotation

```common-lisp
@synchronized-with
(defun genid-with-sync ()
  "generate id with synchronized"
  (sleep 1)
  (get-universal-time))
```

## Installation

```common-lisp
(load "synchronized.lisp")
```

This library will be available on Quicklisp when ready for use.

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)

## License

Copyright &copy; 2017 tamura shingo Licensed under the MIT License.
