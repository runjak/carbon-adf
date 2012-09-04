SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL';

CREATE SCHEMA IF NOT EXISTS `OpenBrain` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci ;
USE `OpenBrain` ;

-- -----------------------------------------------------
-- Table `OpenBrain`.`DiscussionInfo`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`DiscussionInfo` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`DiscussionInfo` (
  `discussionid` INT(11) NOT NULL AUTO_INCREMENT ,
  `complete` INT(11) NULL ,
  `deadline` INT(11) NOT NULL ,
  PRIMARY KEY (`discussionid`) ,
  UNIQUE INDEX `_UNIQUE` (`discussionid` ASC) ,
  INDEX `fk_DiscussionInfo_1` (`complete` ASC) ,
  CONSTRAINT `fk_DiscussionInfo_1`
    FOREIGN KEY (`complete` )
    REFERENCES `OpenBrain`.`Information` (`informationid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`Media`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`Media` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`Media` (
  `mediaid` INT(11) NOT NULL AUTO_INCREMENT ,
  `content` TEXT NULL ,
  `collectiontype` INT(11) NULL ,
  `discussionid` INT(11) NULL ,
  PRIMARY KEY (`mediaid`) ,
  UNIQUE INDEX `mediaid_UNIQUE` (`mediaid` ASC) ,
  INDEX `fk_Media_1_idx` (`discussionid` ASC) ,
  CONSTRAINT `MediaToDiscussionInfo`
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
  `creation` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ,
  `deletion` TIMESTAMP NULL ,
  `description` TEXT NULL ,
  `title` VARCHAR(255) NOT NULL DEFAULT '' ,
  `mediaid` INT(11) NOT NULL ,
  PRIMARY KEY (`informationid`) ,
  UNIQUE INDEX `informationid_UNIQUE` (`informationid` ASC) ,
  INDEX `userid_idx` (`author` ASC) ,
  INDEX `mediaid_idx` (`mediaid` ASC) ,
  CONSTRAINT `InformationToUserData`
    FOREIGN KEY (`author` )
    REFERENCES `OpenBrain`.`UserData` (`userid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `InformationToMedia`
    FOREIGN KEY (`mediaid` )
    REFERENCES `OpenBrain`.`Media` (`mediaid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `OpenBrain`.`UserData`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `OpenBrain`.`UserData` ;

CREATE  TABLE IF NOT EXISTS `OpenBrain`.`UserData` (
  `userid` INT(11) NOT NULL AUTO_INCREMENT ,
  `username` VARCHAR(255) NOT NULL ,
  `password` VARCHAR(255) NOT NULL ,
  `karma` INT UNSIGNED NOT NULL DEFAULT 0 ,
  `creation` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ,
  `lastLogin` TIMESTAMP NULL ,
  `isAdmin` TINYINT(1) NOT NULL DEFAULT 0 ,
  `salt` VARCHAR(255) NOT NULL ,
  `actionKey` VARCHAR(255) NULL ,
  `profile` INT(11) NULL ,
  PRIMARY KEY (`userid`) ,
  UNIQUE INDEX `userid_UNIQUE` (`userid` ASC) ,
  INDEX `fk_UserData_1` (`profile` ASC) ,
  CONSTRAINT `fk_UserData_1`
    FOREIGN KEY (`profile` )
    REFERENCES `OpenBrain`.`Information` (`informationid` )
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
  CONSTRAINT `DiscussionChoicesToDiscussionInfo`
    FOREIGN KEY (`discussionid` )
    REFERENCES `OpenBrain`.`DiscussionInfo` (`discussionid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `DiscussionChoicesToInformation`
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
  CONSTRAINT `DiscussionParticipantsToDiscussionInfo`
    FOREIGN KEY (`discussionid` )
    REFERENCES `OpenBrain`.`DiscussionInfo` (`discussionid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `DiscussionParticipantsToUserData`
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
  `creation` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ,
  `deletion` TIMESTAMP NULL ,
  `type` INT(11) NOT NULL ,
  `source` INT(11) NOT NULL ,
  `target` INT(11) NOT NULL ,
  PRIMARY KEY (`relationid`) ,
  UNIQUE INDEX `relationid_UNIQUE` (`relationid` ASC) ,
  INDEX `informationid_idx` (`source` ASC) ,
  INDEX `informationid_idx1` (`target` ASC) ,
  CONSTRAINT `RelationsSourceToInformation`
    FOREIGN KEY (`source` )
    REFERENCES `OpenBrain`.`Information` (`informationid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `RelationsTargetToInformation`
    FOREIGN KEY (`target` )
    REFERENCES `OpenBrain`.`Information` (`informationid` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
