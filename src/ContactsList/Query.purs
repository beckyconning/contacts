module ContactsList.Query where

import Prelude

data ListQuery next = Search String next
                    | PresentContact String next
                    | CreateContact next

