Step-by-step Instruction to 
Use LaTex and R to Write a Paper 
on Linus OS and utilizing github
For Linux users :
1. Install git from Ubuntu Software Center by search “r-base”. Otherwise, in the terminal
sudo apt-get update
sudo apt-get install r-base
sudo apt-get install r-base-dev
2. Install R Studio from Ubuntu Software Center by search “rstudio”. Otherwise, in the terminal
sudo apt-get install rstudio
3. Open github.com, register an acount. You will get
user.name (user_name)
user.email (user_email)
password
4. In the browser, create a repository named as RepositoryName. 
Your repository will have an url like https://github.com/user_name/RepositoryName
5. Back to Ubuntu terminal. In fresh open terminal, make a directory as RepositoryName. 
mkdir RepositoryName 
cd  RepositoryName
6. For example, use Ubuntu Files application to copy README.md, file1.Rnw, file1.pdf and bib1.bib to the folder you just created (/home/RepositoryName)
7. First time to use git on Ubuntu, You need run following commands in terminal 
(optional) cd RepositoryName
git config --global user.email user_email
git config --global user.name user_name
git init
git remote add https://github.com/user_name/RepositoryName.git
git add *
git commit -m “your first commit message here”
git pull -u origin master
8. Right now, you can open github.com to check your repository content at
https://github.com/user_name/RepositoryName
You are seeing four (4) files (README.md, file1.Rnw, file1.pdf and bib1.bib)  
9. In another Linus computer, you would run following commands with terminal to pull all contents from github repository which you just newly created and uploaded with four (4) files.
mkdir RepositoryName 
cd  RepositoryName
it config --global user.email user_email
git config --global user.name user_name
git init
git remote add https://github.com/user_name/RepositoryName.git
git pull origin master
10. See right now there is four (4) files on your local.
