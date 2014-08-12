module DbConnect (dbConnectInfo) where

import Database.PostgreSQL.Simple

dbConnectInfo :: ConnectInfo
dbConnectInfo = ConnectInfo "ec2-54-197-238-8.compute-1.amazonaws.com" 5432 "wqjjxnyklchrwn" "4s4YvSJclwr1pUQMo36p4rv4At" "db86ci3toitofh"

--postgres://wqjjxnyklchrwn:4s4YvSJclwr1pUQMo36p4rv4At@ec2-54-197-238-8.compute-1.amazonaws.com:5432/db86ci3toitofh