SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

CREATE SCHEMA IF NOT EXISTS `OpenBrain` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci ;
USE `OpenBrain` ;

-- -----------------------------------------------------
-- Table `OpenBrain`.`UserData`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`UserData` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`UserData` (
  `userid` INT(11) NOT NULL AUTO_INCREMENT ,
  `username` VARCHAR(255) NOT NULL ,
  `password` VARCHAR(255) NOT NULL ,
  `karma` INT UNSIGNED NOT NULL DEFAULT 0 ,
  `creation` INT(11) NOT NULL ,
  `lastLogin` INT(11) NOT NULL ,
  `isAdmin` TINYINT(1) NOT NULL DEFAULT 0 ,
  `salt` VARCHAR(255) NOT NULL ,
  `actionKey` VARCHAR(255) NOT NULL DEFAULT '' ,
  PRIMARY KEY (`userid`) ,
  UNIQUE INDEX `userid_UNIQUE` (`userid` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`Profile`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`Profile` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`Profile` (
  `profileid` INT(11) NOT NULL AUTO_INCREMENT ,
  `userid` INT(11) NOT NULL ,
  `accessRule` TINYINT UNSIGNED NOT NULL DEFAULT 2 COMMENT 'Default 2\\ncorresponds to\\nthe Enum value\\nof data AccessRule\\nNone' ,
  `avatar` TEXT NOT NULL DEFAULT '' ,
  `name_prefix` VARCHAR(255) NOT NULL DEFAULT '' ,
  `name_foreName` VARCHAR(255) NOT NULL DEFAULT '' ,
  `name_middleName` VARCHAR(255) NOT NULL DEFAULT '' ,
  `name_familyName` VARCHAR(255) NOT NULL DEFAULT '' ,
  `name_suffix` VARCHAR(255) NOT NULL DEFAULT '' ,
  PRIMARY KEY (`profileid`) ,
  UNIQUE INDEX `profileid_UNIQUE` (`profileid` ASC) ,
  INDEX `fk_Profile_1_idx` (`userid` ASC) ,
  CONSTRAINT `userid`
    FOREIGN KEY (`userid` )
    REFERENCES `OpenBrain`.`UserData` (`userid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`Location`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`Location` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`Location` (
  `locationid` INT(11) NOT NULL AUTO_INCREMENT ,
  `profileid` INT(11) NOT NULL ,
  `street` VARCHAR(255) NOT NULL DEFAULT '' ,
  `city` VARCHAR(255) NOT NULL DEFAULT '' ,
  `state` VARCHAR(255) NOT NULL DEFAULT '' ,
  `land` VARCHAR(255) NOT NULL DEFAULT '' ,
  `zipCode` VARCHAR(255) NOT NULL DEFAULT '' ,
  `note` TEXT NOT NULL DEFAULT '' ,
  PRIMARY KEY (`locationid`) ,
  UNIQUE INDEX `locationid_UNIQUE` (`locationid` ASC) ,
  UNIQUE INDEX `profileid_UNIQUE` (`profileid` ASC) ,
  INDEX `profileid_idx` (`profileid` ASC) ,
  CONSTRAINT `profileid`
    FOREIGN KEY (`profileid` )
    REFERENCES `OpenBrain`.`Profile` (`profileid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`ProfileSnippet`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`ProfileSnippet` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`ProfileSnippet` (
  `profilesnippetid` INT(11) NOT NULL AUTO_INCREMENT ,
  `profileid` INT(11) NOT NULL ,
  `title` VARCHAR(255) NOT NULL DEFAULT '' ,
  `description` TEXT NOT NULL DEFAULT '' ,
  `target` TEXT NOT NULL ,
  `snippetType` TINYINT NOT NULL ,
  PRIMARY KEY (`profilesnippetid`) ,
  UNIQUE INDEX `profilesnippetid_UNIQUE` (`profilesnippetid` ASC) ,
  INDEX `profileid_idx` (`profileid` ASC) ,
  CONSTRAINT `profileid`
    FOREIGN KEY (`profileid` )
    REFERENCES `OpenBrain`.`Profile` (`profileid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`DiscussionInfo`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`DiscussionInfo` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`DiscussionInfo` (
  `discussionid` INT(11) NOT NULL AUTO_INCREMENT ,
  `complete` INT(11) NULL ,
  `deadline` INT(11) NOT NULL ,
  PRIMARY KEY (`discussionid`) ,
  UNIQUE INDEX `_UNIQUE` (`discussionid` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`Media`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`Media` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`Media` (
  `mediaid` INT(11) NOT NULL AUTO_INCREMENT ,
  `content` TEXT NULL DEFAULT '' ,
  `collectiontype` INT(11) NULL ,
  `discussionid` INT(11) NULL ,
  PRIMARY KEY (`mediaid`) ,
  UNIQUE INDEX `mediaid_UNIQUE` (`mediaid` ASC) ,
  INDEX `fk_Media_1_idx` (`discussionid` ASC) ,
  CONSTRAINT `discussionid`
    FOREIGN KEY (`discussionid` )
    REFERENCES `OpenBrain`.`DiscussionInfo` (`discussionid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`Information`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`Information` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`Information` (
  `informationid` INT(11) NOT NULL AUTO_INCREMENT ,
  `author` INT(11) NOT NULL ,
  `creation` INT(11) NOT NULL ,
  `description` TEXT NOT NULL DEFAULT '' ,
  `title` VARCHAR(255) NOT NULL DEFAULT '' ,
  `mediaid` INT(11) NOT NULL ,
  PRIMARY KEY (`informationid`) ,
  UNIQUE INDEX `informationid_UNIQUE` (`informationid` ASC) ,
  INDEX `userid_idx` (`author` ASC) ,
  INDEX `mediaid_idx` (`mediaid` ASC) ,
  CONSTRAINT `userid`
    FOREIGN KEY (`author` )
    REFERENCES `OpenBrain`.`UserData` (`userid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `mediaid`
    FOREIGN KEY (`mediaid` )
    REFERENCES `OpenBrain`.`Media` (`mediaid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`DiscussionChoices`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`DiscussionChoices` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`DiscussionChoices` (
  `discussionid` INT(11) NOT NULL ,
  `informationid` INT(11) NOT NULL ,
  `votes` INT(11) NOT NULL DEFAULT 0 ,
  INDEX `discussionid_idx` (`discussionid` ASC) ,
  INDEX `informationid_idx` (`informationid` ASC) ,
  CONSTRAINT `discussionid`
    FOREIGN KEY (`discussionid` )
    REFERENCES `OpenBrain`.`DiscussionInfo` (`discussionid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `informationid`
    FOREIGN KEY (`informationid` )
    REFERENCES `OpenBrain`.`Information` (`informationid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`DiscussionParticipants`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`DiscussionParticipants` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`DiscussionParticipants` (
  `discussionid` INT(11) NOT NULL ,
  `voted` TINYINT(1) NOT NULL DEFAULT FALSE ,
  `userid` INT(11) NOT NULL ,
  INDEX `discussionid_idx` (`discussionid` ASC) ,
  INDEX `userid_idx` (`userid` ASC) ,
  CONSTRAINT `discussionid`
    FOREIGN KEY (`discussionid` )
    REFERENCES `OpenBrain`.`DiscussionInfo` (`discussionid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `userid`
    FOREIGN KEY (`userid` )
    REFERENCES `OpenBrain`.`UserData` (`userid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`Relations`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`Relations` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`Relations` (
  `relationid` INT(11) NOT NULL AUTO_INCREMENT ,
  `comment` VARCHAR(255) NOT NULL ,
  `creation` INT(11) NOT NULL ,
  `deletion` INT(11) NULL DEFAULT NULL ,
  `type` INT(11) NOT NULL ,
  `source` INT(11) NOT NULL ,
  `target` INT(11) NOT NULL ,
  PRIMARY KEY (`relationid`) ,
  UNIQUE INDEX `relationid_UNIQUE` (`relationid` ASC) ,
  INDEX `informationid_idx` (`source` ASC) ,
  INDEX `informationid_idx1` (`target` ASC) ,
  CONSTRAINT `informationid`
    FOREIGN KEY (`source` )
    REFERENCES `OpenBrain`.`Information` (`informationid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `informationid`
    FOREIGN KEY (`target` )
    REFERENCES `OpenBrain`.`Information` (`informationid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
