-- {-# LANGUAGE DataKinds           #-}
-- {-# LANGUAGE DeriveAnyClass      #-}
-- {-# LANGUAGE DeriveGeneric       #-}
-- {-# LANGUAGE FlexibleContexts    #-}
-- {-# LANGUAGE NoImplicitPrelude   #-}
-- {-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell     #-}
-- {-# LANGUAGE TypeApplications    #-}
-- {-# LANGUAGE TypeFamilies        #-}
-- {-# LANGUAGE TypeOperators       #-}
-- {-# LANGUAGE NumericUnderscores  #-}
-- {-# LANGUAGE NamedFieldPuns      #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Contract where

import qualified Ledger.Typed.Scripts   as Scripts



{-# INLINABLE mkValidator #-}
mkValidator :: DatumType -> RedeemerType -> ScriptContext -> Bool
mkValidator _ _ _ = undefined

typedValidator :: Scripts.TypedValidator

mkValidator
