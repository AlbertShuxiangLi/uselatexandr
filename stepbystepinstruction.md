---
title: "Use LaTex and R to Write a Paper"
author: "Albert Shuxiang Li"
date: "25 July 2015"
output
  html_document:
        keep_md: yes
---
#### Step-by-step Instruction, Utilizing Latex, r, Ubuntu and github
#### For Linux users (for example, Ubuntu 15.04)
- Install texlive from Ubuntu Software Center by search “texlive”. Otherwise, in the terminal
      - sudo apt-get install texlive-full
      - sudo apt-get install biber (optional)
      - sudo apt-get install babel (optional)
      - sudo apt-get install texworks (recommended)
- Install git from Ubuntu Software Center by search “r-base”. Otherwise, in the terminal
      - sudo apt-get update
      - sudo apt-get install r-base
      - sudo apt-get install r-base-dev
- Install R Studio using Ubuntu Software Center
      - Open RStudio Desktop download page, clik link 
      - choose option with Ubuntu Software Center
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
      - cd RepositoryName (optional, if you are not here yet)
      - git config --global user.email "user_email"
      - git config --global user.name "user_name"
      - git init
      - git remote add https://github.com/user_name/RepositoryName.git
      - git add *
      - git commit -m “your first commit message here”
      - git push -u https://github.com/user_name/RepositoryName master
- Right now, you can open github.com to check your repository content at
      - https://github.com/user_name/RepositoryName
      - You are seeing four (4) files (README.md, file1.Rnw, file1.pdf and bib1.bib)  
- In another Linux computer, you would run following commands with terminal to pull all contents from github repository which you just newly created and uploaded with four (4) files.
      - mkdir RepositoryName 
      - cd  RepositoryName
      - git config --global user.email "user_email"
      - git config --global user.name "user_name"
      - git init
      - git remote add https://github.com/user_name/RepositoryName.git
      - git pull -u https://github.com/user_name/RepositoryName master
      - See right now there are four (4) files on your local.
