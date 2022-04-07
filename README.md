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
    $ stack build


# Getting started
## 1. Create database
Application use PostgreSQL as database.

Before start using app you should create new database and enter details in [Configuration](#2-configuration)

There is database diagram
![](https://github.com/JekaGrib/lola/raw/master/other/pic/dbDiaram.png)

To create database structure you can use file "dbStructure.sql"

## 2. Configuration
Before start, you should rename "example.config" to "bot.config". 
Then you should make changes in this file.

There is table with descriptions of each values in configuration file, that should be replace to your values.
