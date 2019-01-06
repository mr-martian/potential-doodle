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
                       (barnode DP () (morph pronoun () you)))))))
(format t "~a" sen)
(format t "~a" (apply-rules (get 'english 'syntax) sen))
