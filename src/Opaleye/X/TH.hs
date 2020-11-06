{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Opaleye.X.TH
  ( HasDefault, NonNullable, NullableDefault, Nullable
  , Interpretation, TypeNamer
  , mkTable, mkTable'
  , mkTypes
  ) where

import Control.Monad              (foldM, replicateM)
import Data.List.Split            (splitOn)
import X.Prelude                  (capitalise, dashify, uncapitalise, unprefix)

--import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarBangType)
import Opaleye                    (Field, FieldNullable, Table)

-- | Descriptor for a "not null" column
data NonNullable r w

-- | Descriptor for a column that can be null
data Nullable r w

-- | Descriptor for a column with default values
data HasDefault r w

-- | Descriptor for a column that can be null with default values
data NullableDefault r w

-- Descriptors as internal type
data ContraintType = NonNullableConst -- NonNullable
                   | NullableConst  -- Nullable
                   | DefaultConst  -- HasDefault
                   | NullableDefaultConst -- NullDefault

-- internal dictionary for computation
data Env = Env
  { typeName :: Name
  , typeNameStr :: String
  , typeConstructor :: Name
  , fields :: [(Name, Bang, Type)]
  , derivations :: [DerivClause]
  } deriving Show

-- | Interpretation modes
data Interpretation = PostgresW -- ^ Interpretation for opaleye to write into postgres
                    | PostgresR -- ^ Interpretation for opaleye to read from postgres
                    | HaskW -- ^ Interpretation for users to write to opaleye
                    | HaskR -- ^ Interpretation for users to read from opaleye
-- | Function to translate the model name for each interpretation mode
type TypeNamer = Interpretation -> String -> String

-- | Declare opaleye types using a single record declaration.
-- For example:
--
-- > $(declareRecords
-- >   [d| data Route = Route
-- >       { id :: 'HasDefault' Int PGInt4
-- >       , createdAt :: 'HasDefault' UTCTime PGTimestamptz
-- >       , duration :: 'Nullable' String PGText
-- >       , terminal :: 'NonNullable' Int PGInt4
-- >       , destination :: 'NonNullable' Text PGText
-- >       }
-- >   |])
--
-- This translates to:
--
-- >  data Route a_a8Jc a_a8Jd a_a8Je a_a8Jf a_a8Jg
-- >   = Route {routeId :: a_a8Jc,
-- >            routeCreatedAt :: a_a8Jd,
-- >            routeDuration :: a_a8Je,
-- >            routeTerminal :: a_a8Jf,
-- >            routeDestination :: a_a8Jg}
-- > type RouteR = Route Int UTCTime (Maybe String) Int Text
-- > type RouteW =
-- >     Route (Maybe Int) (Maybe UTCTime) (Maybe String) Int Text
-- > type RoutePR =
-- >     Route (Field PGInt4) (Field PGTimestamptz) (FieldNullable PGText) (Field PGInt4) (Field PGText)
-- > type RoutePW =
-- >     Route (Maybe (Field PGInt4)) (Maybe (Field PGTimestamptz)) (FieldNullable PGText) (Field PGInt4) (Field PGText)
mkTypes :: Q [Dec] -> Q [Dec]
mkTypes baseRecords = concat <$> (mapM mkType =<< baseRecords)


-- | Generates a table definition for a polymorphic type generated by @mkType@ and
-- an adaptor generated by makeAdaptorAndInstance
-- For example:
--
-- > $('mkTable' "my_route" "pRoute" Route'')@
--
-- translates to
--
-- > routeTable :: Table RoutePW RoutePR
-- >  routeTable = table "route" (pRoute Route
-- >   { routeId = tableField "id"
-- >   , routeCreatedAt = tableField "created_at"
-- >   , routeDuration = tableField "duration"
-- >   , routeTerminal = tableField "terminal"
-- >   , routeDestination = tableField "destination"
-- >   })
--
-- Notice that it uses @'dashify'@ on the field names.
mkTable :: String -> String -> Name -> Q [Dec]
mkTable tableName profunctor typeName = do
  typeInfo <- reify typeName
  let (typeNameStr, constructor, fields) = getCons typeInfo
  tableSig <- mkTableSignature typeNameStr
  tableDefn <- mkTableBody profunctor tableName typeNameStr constructor fields
  return $ tableSig : [ValD (VarP $ haskTableName typeNameStr) (NormalB tableDefn) []]
  where
    getCons (TyConI (DataD _ name _ _ [RecC constructor fields] _)) =
      (nameBase name, constructor, map getFieldName fields)
    getCons _                                                       =
      error "Expected a type"
    getFieldName (n, _, _) = n

-- | Like @'mkTable'@ but the table name is extracted from the @Name@ passed
-- with @'dashify'@ applied to it.
mkTable' :: String -> Name -> Q [Dec]
mkTable' profunctor typeName = mkTable tableName profunctor typeName
  where tableName = dashify $ nameBase typeName

mkType :: Dec -> Q [Dec]
mkType decl = do
  let env        = extractEnv decl
      synCreator = mkTypeSynonym defaultTypeNamer env
  let declarations =
        [ mkPolymorhicRecord env
        , synCreator HaskR
        , synCreator HaskW
        , synCreator PostgresR
        , synCreator PostgresW
        ]
  sequence declarations

extractEnv :: Dec -> Env
extractEnv (DataD _ name _ _ [RecC cons fields] derivations) =
  Env cleanName cleanNameStr consName fields derivations
  where
    consName =  mkName $ stripRandomizer cons
    cleanNameStr = stripRandomizer name
    cleanName = mkName cleanNameStr
extractEnv _                                = error "Expected data declaration"

defaultTypeNamer :: TypeNamer
defaultTypeNamer PostgresW = (++ "PW")
defaultTypeNamer PostgresR = (++ "PR")
defaultTypeNamer HaskW     = (++ "W")
defaultTypeNamer HaskR     = (++ "R")

mkTypeSynonym :: TypeNamer -> Env -> Interpretation -> Q Dec
mkTypeSynonym namer Env { typeNameStr, typeConstructor, fields } interpretation = do
  let synName   = mkName $ namer interpretation typeNameStr
      synTyCons = ConT typeConstructor
  synType <- mkTypeSynType synTyCons interpretation fields
  return $ TySynD synName [] synType

mkPolymorhicRecord :: Env -> Q Dec
mkPolymorhicRecord Env { typeNameStr, typeName, fields, typeConstructor, derivations } = do
  polyVars <- replicateM (length fields) (newName "a")
  let conTypeVars = map PlainTV polyVars
      polyFields  = zipWith polify polyVars fields
  return $ DataD [] typeName conTypeVars Nothing [RecC typeConstructor polyFields] derivations
  where
    recField prefix = mkName . (prefix ++) . capitalise . stripRandomizer
    polyRecField = recField (uncapitalise typeNameStr)
    polify varName (fieldName, bang', _) = (polyRecField fieldName, bang', VarT varName)

mkTableSignature :: String -> Q Dec
mkTableSignature typeNameStr = sigD (haskTableName typeNameStr) [t|Table $(typeSyn PostgresW) $(typeSyn PostgresR)|]
  where typeSyn = conT . mkName . flip defaultTypeNamer typeNameStr

mkTableBody :: String -> String -> String -> Name -> [Name] -> Q Exp
mkTableBody profunctor tableName typeNameStr consName fieldNames =
  [e|table $(stringExp tableName) ($(pfExp profunctor) $(columnsDef))|]
  where
    columnsDef = recConE consName (map fieldExp' fieldNames)
    fieldExp' x = fieldExp x [e|tableField $(pgField x)|]
    stringExp = litE . StringL
    pfExp = varE . mkName
    pgField = stringExp . noKeywords . dashify . unprefix typeNameStr . nameBase

noKeywords :: String -> String
noKeywords w
  | last w == '_' = init w
  | otherwise     = w

haskTableName :: String -> Name
haskTableName = mkName . (++ "Table") . uncapitalise

stripRandomizer :: Name -> String
stripRandomizer = head . splitOn "_" . nameBase

mkTypeSynType :: Type -> Interpretation -> [VarBangType] -> Q Type
mkTypeSynType synTyCons interpretation fields =
  foldM appT' synTyCons =<< mapM interpreteField fields
  where
    interpreteField (_, _, AppT (AppT desc rType) wType) =
      interpreteDescriptor interpretation (fromDescriptor desc) rType wType
    interpreteField _                                    =
      error "Expected descriptor(Nullable, NonNullable...)"
    appT' m = return . AppT m

interpreteDescriptor :: Interpretation -> ContraintType -> Type -> Type -> Q Type
interpreteDescriptor HaskR NullableConst hType _            = [t|Maybe $(return hType)|]
interpreteDescriptor HaskR NullableDefaultConst hType _     = [t|Maybe $(return hType)|]
interpreteDescriptor HaskR _ hType _                        = return hType
interpreteDescriptor HaskW NonNullableConst hType _         = return hType
interpreteDescriptor HaskW _ hType _                        = [t|Maybe $(return hType)|]
interpreteDescriptor PostgresR NullableConst _ pType        = [t|FieldNullable $(return pType)|]
interpreteDescriptor PostgresR NullableDefaultConst _ pType = [t|FieldNullable $(return pType)|]
interpreteDescriptor PostgresR _ _ pType                    = [t|Field $(return pType)|]
interpreteDescriptor PostgresW NonNullableConst _ pType     = [t|Field $(return pType)|]
interpreteDescriptor PostgresW NullableConst _ pType        = [t|FieldNullable $(return pType)|]
interpreteDescriptor PostgresW DefaultConst _ pType         = [t|Maybe (Field $(return pType))|]
interpreteDescriptor PostgresW NullableDefaultConst _ pType = [t|Maybe (FieldNullable $(return pType))|]

fromDescriptor :: Type -> ContraintType
fromDescriptor (ConT desc)
  | descStr == "NonNullable" = NonNullableConst
  | descStr == "Nullable"    = NullableConst
  | descStr == "HasDefault"  = DefaultConst
  | descStr == "NullDefault" = NullableConst
  | otherwise             = error "Uninterpretable Descriptor"
  where descStr = nameBase desc
fromDescriptor _           = error "Uninterpretable Descriptor"
