---
title: "Use LaTex and R to Write a Paper (Step-by-step Instruction, Utilizing Ubuntu and github)"
author: "Albert Shuxiang Li"
date: "25 July 2015"
output:
  html_document:
        keep_md: yes
---
## For Linux users
- Install livetex from Ubuntu Software Center by search “texlive”. Otherwise, in the terminal
      - sudo apt-get install texlive
- Install git from Ubuntu Software Center by search “r-base”. Otherwise, in the terminal
      - sudo apt-get update
      - sudo apt-get install r-base
      - sudo apt-get install r-base-dev
- Install R Studio from Ubuntu Software Center by search “rstudio”. Otherwise, in the terminal
      - sudo apt-get install rstudio
- Open github.com, register an acount. You will get
      - user.name (user_name)
      - user.email (user_email)
      - password
- In the browser, create a repository named as RepositoryName. 
      - Your repository will have an url like https://github.com/user_name/RepositoryName
- Back to Ubuntu terminal. In fresh open terminal, make a directory as RepositoryName. 
      - mkdir RepositoryName 
      - cd  RepositoryName
- For example, use Ubuntu Files application to copy README.md, file1.Rnw, file1.pdf and bib1.bib to the folder you just created (/home/RepositoryName)
- First time to use git on Ubuntu, You need run following commands in terminal 
      - (optional) cd RepositoryName
      - git config --global user.email user_email
      - git config --global user.name user_name
      - git init
      - git remote add https://github.com/user_name/RepositoryName.git
      - git add *
      - git commit -m “your first commit message here”
      - git pull -u origin master
- Right now, you can open github.com to check your repository content at
      - https://github.com/user_name/RepositoryName
      - You are seeing four (4) files (README.md, file1.Rnw, file1.pdf and bib1.bib)  
- In another Linus computer, you would run following commands with terminal to pull all contents from github repository which you just newly created and uploaded with four (4) files.
      - mkdir RepositoryName 
      - cd  RepositoryName
      - git config --global user.email user_email
      - git config --global user.name user_name
      - git init
      - git remote add https://github.com/user_name/RepositoryName.git
      - git pull origin master
      - See right now there is four (4) files on your local.
