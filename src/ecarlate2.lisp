(in-package :lol)


(defclass dana (danser)
  ()
  (:default-initargs
    :name 'dana))

(defclass carole (danser)
  ()
  (:default-initargs
    :name 'carole))


(defclass cl-user::bodypart ()
  ())


(defclass durée (dimension)
  ()
  (:default-initargs
    :name 'durée
    :type :continuous
    :datum 0
    :values-set '(0 1000)))


(defclass orientation (dimension)
  ()
  (:default-initargs
    :name 'orient
    :type :crispy
    :datum 'w!
    :values-set '(w! P 1 1.5 2 2.5 3 3.5 4 4.5 5 5.5 6 6.5 7 7.5 8 8.5)))


(defclass plan (dimension)
  ()
  (:default-initargs
    :name 'plan
    :type :crispy
    :datum 1
    :values-set '(1 2 3 4 5)))


(defclass duree (dimension)
  ()
  (:default-initargs
    :name 'plan
    :type :crispy
    :datum 1
    :values-set '(1 2 3 4 5)))

(defclass point (dimension)
  ()
  (:default-initargs
    :name 'point
    :type :crispy
    :values-set '(w! 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
    :datum 'w!
    ))

(defclass temps (dimension)
  ()
  (:default-initargs
    :name 'temps
    :type :crispy
    :datum 'w!
    :values-set
    '(w! 1 2 3 4 5 6 7 8)))


(defclass inclin (dimension)
  ()
  (:default-initargs
    :name 'inclinaison
    :type :crispy
    :datum 'w!
    :values-set '(w! -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6)))

(defclass  respiration (bodypart)
  ()
  )

(defclass regard (bodypart)
  ()
  )

(defclass visage (bodypart)
  ()
  )


(defclass pupilles (bodypart)
  ()
  )

;; parties du corps (Dana)


(defclass - (bodypart)
  ())


(defclass boutdoigtsG (bodypart)
  ())

(defclass boutdoigtsD (bodypart)
  ())

(defclass mainG (bodypart)
  ())


(defclass mainD (bodypart)
  ())


(defclass tete (bodypart)
  ())


(defclass coudeGch (bodypart)
  ())


(defclass coudeDRT (bodypart)
  ())


(defclass jambeD (bodypart)
  ())


(defclass jambeG (bodypart)
  ())


(defclass jambe (bodypart)
  ())

(defclass AVANT.B.D (bodypart)
  ())


(defclass AVANT.B.G (bodypart)
  ())


(defclass BRASD (bodypart)
  ())


(defclass BRASG (bodypart)
  ())


(defclass BRAS (bodypart)
  ())

(defclass jambe (bodypart)
  ())


(defclass SURFACE-A (bodypart)
  ())


(defclass BUSTE (bodypart)
  ())

(defclass BASSIN (bodypart)
  ())

(defclass tronc (bodypart)
  ())


(defclass HANCHE-G (bodypart)
  ())

(defclass HANCHE-D (bodypart)
  ())


(defclass HANCHEG (bodypart)
  ())

(defclass HANCHED (bodypart)
  ())


(defclass HANCHE (bodypart)
  ())


(defclass COUDE (bodypart)
  ())


(defclass BORDINTPDRT (bodypart)
  ())


(defclass BORDEXTPDRT (bodypart)
  ())


(defclass POIGNET (bodypart)
  ())


(defclass pouceg (bodypart)
  ())


(defclass pouced (bodypart)
  ())


(defclass périnée (bodypart)
  ())

(defclass SURFACE-B-D (bodypart)
  ())

(defclass pouce (bodypart)
  ())


(defclass talonG (bodypart)
  ())

(defclass talonD (bodypart)
  ())


(defclass membres (bodypart)
  ())

(defclass 2membres (bodypart)
  ())

(defclass trochanter (bodypart)
  ())


(defclass orteilG (bodypart)
  ())


(defclass orteilD (bodypart)
  ())

(defclass plantepied (bodypart)
  ())

(defclass DESSUSMAING (bodypart)
  ())

(defclass ARRETEGDUBASSIN (bodypart)
  ())


(defclass SURFACEPTRONC (bodypart)
  ())

(defclass SURFACE-c (bodypart)
  ())

(defclass COUDEDRT (bodypart)
  ())

(defclass BASDEJBEDRT (bodypart)
  ())

(defclass BASDEJBEGCH (bodypart)
  ())


(defclass SURFACEATRONC (bodypart)
  ())


(defclass DOIGTSD (bodypart)
  ())

(defclass SURFACE-D (bodypart)
  ())

(defclass SURFACEDD (bodypart)
  ())


(defclass GENOUD (bodypart)
  ())


(defclass GENOUG (bodypart)
  ())


(defclass GENOU (bodypart)
  ())


(defclass PLANTES (bodypart)
  ())


(defclass PAUMEG (bodypart)
  ())


(defclass PAUMED (bodypart)
  ())


(defclass TRANCHEINTPIED (bodypart)
  ())


(defclass TRANCHEEXTPIED (bodypart)
  ())


(defclass PIEDD (bodypart)
  ())


(defclass PIEDG (bodypart)
  ())




