{-# LANGUAGE QuasiQuotes #-}

module Shell.ConfigurationTest where
import Relude
import qualified Relude.Unsafe as Unsafe
import Test.Tasty ()
import Test.Tasty.Hspec
import Text.RawString.QQ
import Text.URI (render)
import Shell.Configuration 


spec_parseConfiguration :: Spec
spec_parseConfiguration = do
  describe "when the config is valid" $
    it "returns the config" $ do
      cfg <- parseConfiguration validConfiguration
      null (_appConfigEndpoints cfg) `shouldBe` False
  describe "endpoints" $
    it "parses the url correctly" $ do
      cfg <- parseConfiguration validConfiguration
      let endpoint = (Unsafe.head (_appConfigEndpoints cfg))
      render (_endpointURL endpoint)  `shouldBe` "https://example.com/api"
      

validConfiguration :: ByteString
validConfiguration = [r|
endpoints:
  - name: weather
    default: true
    url: "https://example.com/api"
    link: "https://graphql-weather-api.herokuapp.com" 
    http:
      custom-headers:
        - name: "TEST_HEADER"
          value: "TEST_HEADER_VALUE"

themes:
  - name: default
    default: true
    path: /path/to/theme-file
|]
