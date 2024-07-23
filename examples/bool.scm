(= Bool (Set :true :false))

(= not
   (: (=> Bool Bool)
      (? (-> :true :false)
         (-> :false :true))))

(= and
   (: (=> Bool Bool Bool)
      (? (-> :true x x)
         (-> :false _ :false))))
