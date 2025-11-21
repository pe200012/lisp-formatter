(fn cond [...]
  (local clauses [...])
  (if (= clauses nil) nil true
      (let [body []]
        (each [_ clause (ipairs clauses)]
          (if (not (= (length clause) 2)) (error "cond clauses must be pairs")
              true (do
                    (table.insert body (. clause 1))
                    (table.insert body (. clause 2)))))
        `(if ,(table.unpack body)))))