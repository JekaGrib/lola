{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Draft.JSONExample where

import Data.ByteString (ByteString)


json1 :: ByteString
json1 = "{\"draft_name\":\"rock\",\"draft_category_id\": 3,\"draft_text\":\"heyhey\",\"draft_main_pic_id\":42,\"draft_tags_ids\" :[ 15,18,20 ],\"draft_pics_ids\":[ 6,9,12]}"
