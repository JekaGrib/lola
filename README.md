# What is it
Lola is news application.

# How does it work
This application is designed to post and read the news. Read news can everybody. Users need to register to post and comment news. 

News has categories and tags.

## How to make a post

To post news user must be an author. Only admin can make a user an author. 

At first author should create draft and then publish it.

To update post author should create draft of this post and publish it.

# Installing
You can install app with:

    $ git clone https://github.com/JekaGrib/lola.git

And then in app directory:

    $ stack build


# Getting started
## 1. Create database
Application use PostgreSQL as database.

Before start using app you should create new database and enter details in [Configuration](#2-configuration)

There is database diagram
![](https://github.com/JekaGrib/lola/raw/master/other/pic/dbDiagram.png)

To create database structure you can use file "dbStructure.sql"

## 2. Add default entities to db
Application has several default db entities:
1. default picture  - picture of deleted user
2. default user     - deleted user
2. default author   - deleted author
2. default category - deleted category

You can create yours default entities or use or use folder "migrations" with migrations examples.

Aftеr creating entities enter ids in [Configuration](#2-configuration)

## 3. Configuration
Before start, you should rename "example.config" to "postApp.config". 
Then you should make changes in this file.

There is table with descriptions of each values in configuration file, that should be replace to your values.
|Section      | Value                | Description                       | 
|:------------| :------------------- |:------------------------------- |
|Server       | host                 | Server host     |
|Server       | port                 | Server port           |
|Database     | host                 | Database host  |
|Database     | port                 | Database port               |
|Database     | dbname               | Database name               |
|Database     | password             | Database password           |
|defaultValues| defaultPictureId     | Id picture of deleted user  |
|defaultValues| defaultUserId        | Id of deleted user          |
|defaultValues| defaultAuthorId      | Id of deleted author        |
|defaultValues| defaultCategoryId    | Id of deleted category      |
|LimitNumbers | commentNumberLimit   | The number of comments given in the response at a time |
|LimitNumbers | draftNumberLimit     | The number of drafts given in the response at a time |
|LimitNumbers | postNumberLimit      | The number of posts given in the response at a time |
|log          | logLevel             | The logging level is specified here. The log will only display entries of this level and levels above. It can be one of four levels: DEBUG,INFO,WARNING,ERROR. More information [here](#logging)  |

# Logging

There are 4 logging levels from lowest to highest:

1. DEBUG
2. INFO
3. WARNING
4. ERROR

The logging level is specified in [Configuration](#2-configuration). The log will only display entries of this level and levels above.