UserData (
  userid    INTEGER PRIMARY KEY
, username  VARCHAR(255)  NOT NULL
, password  VARCHAR(255)  NOT NULL
, karma     INT UNSIGNED  NOT NULL DEFAULT 0
, creation  INTEGER       NOT NULL
, lastLogin INTEGER       NOT NULL
, isAdmin   TINYINT(1)    NOT NULL DEFAULT 0
)

Profile (
  profileid   INTEGER PRIMARY KEY
, userid      INTEGER
, accessRule  TINYINT UNSIGNED NOT NULL DEFAULT 2
, avatar      TEXT
, FOREIGN KEY(userid) REFERENCES UserData(userid)
)

Name (
  profileid   INTEGER UNIQUE NOT NULL
, prefix      VARCHAR(255) NOT NULL DEFAULT ''
, foreName    VARCHAR(255) NOT NULL DEFAULT ''
, middleName  VARCHAR(255) NOT NULL DEFAULT ''
, familyName  VARCHAR(255) NOT NULL DEFAULT ''
, suffix      VARCHAR(255) NOT NULL DEFAULT ''
, FOREIGN KEY(profileid) REFERENCES Profile(profileid)
)

Location (
  profileid INTEGER       NOT NULL
, street    VARCHAR(255)  NOT NULL DEFAULT ''
, city      VARCHAR(255)  NOT NULL DEFAULT ''
, state     VARCHAR(255)  NOT NULL DEFAULT ''
, land      VARCHAR(255)  NOT NULL DEFAULT ''
, zipCode   VARCHAR(255)  NOT NULL DEFAULT ''
, note      TEXT          NOT NULL DEFAULT ''
, FOREIGN KEY(profileid) REFERENCES Profile(profileid)
)

ProfileSnippet (
  profileid   INTEGER       NOT NULL
, title       VARCHAR(255)  NOT NULL DEFAULT ''
, description TEXT          NOT NULL DEFAULT ''
, target      TEXT          NOT NULL DEFAULT ''
, snippetType TINYINT       NOT NULL
, FOREIGN KEY(profileid) REFERENCES Profile(profileid)
)

Salts (
  salt VARCHAR(255) NOT NULL
, userid INTEGER    NOT NULL
, FOREIGN KEY(userid) REFERENCES UserData(userid)
)

ActionKeys (
  key VARCHAR(255)  NOT NULL
, userid INTEGER    NOT NULL
, FOREIGN KEY(userid) REFERENCES UserData(userid)
)

Information (
  informationid       INTEGER PRIMARY KEY
, author              INTEGER NOT NULL
, creation            INTEGER NOT NULL
, description         TEXT NOT NULL DEFAULT ''
, title               VARCHAR(255) NOT NULL DEFAULT ''
, mediaid             INTEGER NOT NULL
, FOREIGN KEY(author)   REFERENCES UserData(userid)
, FOREIGN KEY(mediaid)  REFERENCES Media(mediaid)
)

Media (
  mediaid         INTEGER PRIMARY KEY
, content         TEXT DEFAULT ''
, collectiontype  INTEGER
, discussionid    INTEGER
, FOREIGN KEY(discussionid) REFERENCES DiscussionInfo(discussionid)
)

DiscussionInfo (
  discussionid  INTEGER PRIMARY KEY
, complete      INTEGER
, deadline      INTEGER NOT NULL
, participants
)

DiscussionChoices (
  discussionid  INTEGER NOT NULL
, informationid INTEGER NOT NULL
, votes         INTEGER NOT NULL DEFAULT 0
, FOREIGN KEY(discussionid)   REFERENCES DiscussionInfo(discussionid)
, FOREIGN KEY(informationid)  REFERENCES Information(informationid)
)

DiscussionParticipants (
  discussionid  INTEGER NOT NULL
, voted         TINYINT(1) NOT NULL DEFAULT 0
, userid        INTEGER NOT NULL
, FOREIGN KEY(discussionid) REFERENCES DiscussionInfo(discussionid)
, FOREIGN KEY(userid)       REFERENCES UserData(userid)
)

Relations (
  relationid INTEGER PRIMARY KEY
, comment  VARCHAR(255) NOT NULL DEFAULT ''
, creation INTEGER      NOT NULL
, deletion INTEGER      DEFAULT NULL
, type     INTEGER      NOT NULL
, source   INTEGER      NOT NULL
, target   INTEGER      NOT NULL
, FOREIGN KEY(source) REFERENCES Information(informationid)
, FOREIGN KEY(target) REFERENCES Information(informationid)
)

