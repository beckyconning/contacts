module Contact.Query where

import Contact.Model

data ContactQuery next = Done Contact next
                       | Back next
                       | Cancel next
                       | EditContact next
                       | ChangeName Contact String next
                       | ChangeTelephone Contact String next
                       | ChangeEmail Contact String next

