import Test.QuickCheck
import Data.Map as Map

data GovDirectory a = GovDirectory {
  mayor :: a,
  interimMayor :: Maybe a,
  cabinet :: Map String a,
  councilMembers :: [a]
}

instance Functor GovDirectory where
  fmap f oldDirectory = GovDirectory {
                          mayor = f (mayor oldDirectory),
                          interimMayor = f <$> interimMayor oldDirectory,
                          cabinet = f <$> cabinet oldDirectory,
                          councilMembers = f <$> councilMembers oldDirectory
                        }

{-| -- Bad example
instance Functor GovDirectory where
  fmap f oldDirectory = GovDirectory {
                          mayor = f (mayor oldDirectory),
                          interimMayor = Nothing,
                          cabinet = f <$> cabinet oldDirectory,
                          councilMembers = f <$> councilMembers oldDirectory
                        }
|-}

instance (Eq a) => Eq (GovDirectory a) where
  (==) g1 g2 = ((&&) ((&&) (mayor g1 == mayor g2) (interimMayor g1 == interimMayor g2)) ((&&) (cabinet g1 == cabinet g2) (councilMembers g1 == councilMembers g2)))

instance (Show a) => Show (GovDirectory a) where
  show directory = show (mayor directory) ++ ", " ++
                    show (interimMayor directory) ++ ", " ++
                    show (cabinet directory) ++ ", " ++
                    show (councilMembers directory)

instance Arbitrary a => Arbitrary (GovDirectory a) where
  arbitrary = do
    m <- arbitrary
    im <- arbitrary
    cab <- arbitrary
    cm <- arbitrary
    return $ GovDirectory
      { mayor = m
      , interimMayor = im
      , cabinet = cab
      , councilMembers = cm }


main :: IO ()
main = quickCheck govDirectoryFunctorCheck

govDirectoryFunctorCheck :: GovDirectory String -> Bool
govDirectoryFunctorCheck gd = fmap id gd == gd

stringCheck :: String -> Bool
stringCheck s = reverse (reverse s) == s
