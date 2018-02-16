{-# LANGUAGE TypeInType #-}

module TypedRecords where
    
    import Data.Kind 

    data Entry a = Entry String a

    infixr 6 :>

    data Dict (a ∷ [Type]) where
        Nil ∷ Dict '[]
        (:>) ∷ Entry a → Dict t → Dict (a ': t)

    myRecord :: Dict '[String, Int]
    myRecord = Entry "name" "Evan" :> Entry "age" 29 :> Nil

