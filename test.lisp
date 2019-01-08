(load "doodle.lisp")
(load "english.lisp")
(setq *lang* 'english)
(setf sen
      (node SEN ()
            (barnode
             CP () nil
             (barnode IP ()
                      (node I ()
                            (morph tense () nonpast))
                      (barnode
                       VP ()
                       (barnode
                        DP ()
                        (morph determiner
                               (:number 'PL) the)
                        (barnode NP () (morph noun () child)
                                 (barnode PP () (morph preposition () of)
                                          (barnode
                                           DP ()
                                           (morph determiner () your)
                                           (barnode-key
                                            NP ()
                                            :spec (barnode
                                                   AP ()
                                                   (morph adjective () elect))
                                            :head (morph noun () sister))))))
                       (morph verb () greet)
                       (barnode DP () (morph pronoun () you))))))
      expected
      (node SEN ()
            (barnode
             CP () nil
             (barnode IP ()
                      (node I ()
                            (morph tense (:person 3 :number 'PL) nonpast))
                      (barnode
                       VP ()
                       (barnode
                        DP ()
                        (morph determiner
                               (:number 'PL) the)
                        (barnode NP () (morph noun () child)
                                 (barnode PP () (morph preposition () of)
                                          (barnode
                                           DP ()
                                           (morph determiner () your)
                                           (barnode-key
                                            NP ()
                                            :spec (barnode
                                                   AP ()
                                                   (morph adjective () elect))
                                            :head (morph noun () sister))))))
                       (morph verb () greet)
                       (barnode DP () (morph pronoun () you)))))))
(format t "~a~%" sen)
(defun compare (t1 t2)
  (labels ((comp (a b)
                 (if (and (listp a) (listp b))
                     (every (lambda (x y)
                              (if (comp x y)
                                  t
                                (progn
                                  (format t "~a does not match ~a~%" a b)
                                  (return-from compare nil))))
                            a b)
                   (equal a b))))
          (comp t1 t2)))
(let ((res (car (apply-rules (get 'english 'syntax) sen))))
  (format t "~a~%" res)
  (format t "Test ~a~%" (if (compare res expected) 'passed 'failed))
  (format t "Linear form: ~a~%" (linearize res)))
(print (process sen))
