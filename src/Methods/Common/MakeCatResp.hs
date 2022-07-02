module Methods.Common.MakeCatResp where

import Api.Response (CatResponse (..), SubCatResponse (..), SuperCatResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT)
import Error (ReqError)
import Logger (LogHandle (..))
import Methods.Common
import Psql.Methods.Common.MakeCatResp
import Psql.Selecty (Cat (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectCats :: CategoryId -> m [Cat],
    selectSubCats :: CategoryId -> m [SubCategoryId]
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectCats' conn)
        (selectSubCats' conn)

makeCatResp ::
  (MonadCatch m) =>
  Handle m ->
  CategoryId ->
  ExceptT ReqError m CatResponse
makeCatResp h@Handle {..} catId = do
  Cat catName superCatId <- catchOneSelectE hLog $ selectCats catId
  subCatsIds <- findOneLevelSubCats h catId
  case superCatId of
    0 ->
      return $ Super $
        SuperCatResponse
          { categoryIdCATR = catId,
            categoryNameCATR = catName,
            subCategoriesCATR = subCatsIds
          }
    _ -> do
      superCatResp <- makeCatResp h superCatId
      return $ Sub $
        SubCatResponse
          { categoryIdSCATR = catId,
            categoryNameSCATR = catName,
            subCategoriesSCATR = subCatsIds,
            superCategorySCATR = superCatResp
          }

findOneLevelSubCats ::
  (MonadCatch m) =>
  Handle m ->
  CategoryId ->
  ExceptT ReqError m [CategoryId]
findOneLevelSubCats Handle {..} catId =
  catchSelE hLog $ selectSubCats catId
