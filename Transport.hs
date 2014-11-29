{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Development.Placeholders

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.SSL

import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL

import Network.HaskellNet.SSL

import Network.HaskellNet.Auth (AuthType(LOGIN))

import qualified Data.ByteString.Char8 as B

data EmailAccount = EmailAccount { 
                    username: String
                  , password: String
                  , host: String
                  , port: Integral 
                  } deriving (Show, Read)   

data Message subject encryptedBody = Message String String

data IncomingCommit = Commit CommitHash EncryptedBody [SigningKey]
data CommitHash = CommitHash string
data EncryptedBody = EncryptedBody string

data Mailbox name = Mailbox String
commits = Mailbox "COMMIT"
snapshots = Mailbox "SNAPSHOTS"
friends = Mailbox "FRIENDS"
mailboxes = [commits snapshots friends]

sendmail :: EmailAccount -> String -> String -> String -> IO ()
sendMail smtpServer subject body recipient = doSMTPSTARTTLS host $ \c -> do
    r@(rsp, _) <- sendCommand c $ AUTH LOGIN username password
    if rsp /= 235
      then print r
      else sendMail username [recipient] mailContent c
  where mailContent = subject `B.append` "\r\n\r\n" `B.append` body
        port = port smtpServer
        host = host smtpServer
        username = username smtpServer
        password = password smtpServer


uidToCommit :: IMAPConnection -> UID -> IO Commit
uidToCommit conn uid = do
  hash <- fetchByString conn uid "BODY[HEADER.FIELDS (SUBJECT)]"
  body <- fetch conn uid 
  (encryptedBody, singingKeys)
  IncomingCommit (CommitHash hash) encryptedBody singingKeys

withImap :: EmailAccount -> (IMAPConnection -> a) -> IO a
withImap imapServer wrapped = do
  c <- connectIMAPSSLWithSettings host cfg
  login c username password
  results <- wrapped c
  logout c
  results
  where cfg = (defaultSettingsWithPort port) { sslMaxLineLength = 100000 }
        username = username imapServer
        password = password imapServer
        host = host imapServer
        port = port imapServer

appendToMailbox :: ImapServer -> String -> IO ()
appendToMailbox imapServer mailboxName = withImap imapServer \conn
    append c MailboxName encryptedBody

findByHash :: IMAPConnection -> ImapServer -> IO [UID]
findByHash conn imapServer = withImap imapServer \conn
    select conn "COMMIT_LOG"
    msgs <- search conn [SUBJECTs ]

read :: IMAPConnection -> UID -> IO ()
read conn uid = do <-
    copy conn uid "COMMIT_LOG"
    store conn uid (PlusFlags Deleted)

getUpdates :: IMAPConnection -> ImapServer -> IO [Commit]
getUpdates conn imapServer = 
    withImap imapServer \conn
      select conn "INBOX"
      msgs <- search conn [ALLs]
      map (read conn) msgs
      expunge conn
      commits <- map uidToCommit msgs

initSystem :: EmailAccount -> IO ()
initSystem account = 
  where keypair = ''
      
-- Todo: make sure all folders there, signatures for all friends in state snapshots, snapshots not more than an epoch behind(will make new snapshot)
validateMailboxes = $unimplemented

createMailbox conn mailbox = do <-

createMailboxes :: IMAPConnection -> IO ()
createMailboxes conn = map (createMailbox conn) mailboxes

-- Removes any more than last 100 state changes, leaves commits 
trim 
