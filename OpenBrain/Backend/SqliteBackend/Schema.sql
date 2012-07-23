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
, description         VARCHAR(1024) NOT NULL DEFAULT ''
, title               VARCHAR(128) NOT NULL DEFAULT ''
, mediacontentid      INTEGER
, discussionid        INTEGER
, FOREIGN KEY(author)         REFERENCES UserData(userid)
, FOREIGN KEY(mediacontentid) REFERENCES MediaContent(mediacontentid)
, FOREIGN KEY(discussionid)   REFERENCES MediaDiscussion(discussionid)
)

MediaContent (
  mediacontentid  INTEGER PRIMARY KEY
, content         TEXT NOT NULL DEFAULT ''
)

MediaBundle(
  informationid INTEGER     NOT NULL
, targetid      INTEGER     NOT NULL
, isdecision    TINYINT(1)  NOT NULL DEFAULT 0
, FOREIGN KEY(informationid)  REFERENCES Information(informationid)
, FOREIGN KEY(targetid)       REFERENCES Information(informationid)
)

MediaDiscussion (
  discussionid  INTEGER PRIMARY KEY
, aftype        INTEGER NOT NULL
, complete      INTEGER
, deadline      INTEGER NOT NULL
, FOREIGN KEY(complete) REFERENCES Information(informationid)
)

DiscussionArguments (
  discussionid  INTEGER NOT NULL
, informationid INTEGER NOT NULL
, FOREIGN KEY(informationid)  REFERENCES Information(informationid)
, FOREIGN KEY(discussionid)   REFERENCES MediaDiscussion(discussionid)
)

DiscussionChoices (
  choiceid      INTEGER PRIMARY KEY
, discussionid  INTEGER NOT NULL
, votecount     INTEGER NOT NULL DEFAULT 0
, FOREIGN KEY(discussionid) REFERENCES MediaDiscussion(discussionid)
)

ChoiceInformations (
  choiceid
, informationid
, FOREIGN KEY(choiceid)       REFERENCES DiscussionChoices(choiceid)
, FOREIGN KEY(informationid)  REFERENCES Information(informationid)
)

DiscussionParticipants (
  discussionid
, userid
, voted TINYINT(1) NOT NULL DEFAULT 0
, FOREIGN KEY(discussionid) REFERENCES MediaDiscussion(discussionid)
, FOREIGN KEY(userid)       REFERENCES UserData(userid)
)

Relations (
  relationid INTEGER PRIMARY KEY
, comment  VARCHAR(512) NOT NULL DEFAULT ''
, creation INTEGER      NOT NULL
, deletion INTEGER      DEFAULT NULL
, type     INTEGER      NOT NULL
, source   INTEGER      NOT NULL
, target   INTEGER      NOT NULL
, FOREIGN KEY(source) REFERENCES Information(informationid)
, FOREIGN KEY(target) REFERENCES Information(informationid)
)

