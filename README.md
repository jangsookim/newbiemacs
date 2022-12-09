# Newbiemacs

Newbiemacs is designed for mathematicians who are new to Emacs.

<img width="761" alt="스크린샷 2022-12-09 오전 9 08 10" src="https://user-images.githubusercontent.com/24665391/206592642-764fc8db-ae97-4b63-a6c9-0cee7e21ca15.png">


# Table of Contents

1.  [Install Newbiemacs on Mac](#orgd216df7)
    1.  [Additional steps for latex on Mac (optional)](#org7fddf1f)
2.  [Install Newbiemacs on Windows](#org60bb9e3)
    1.  [Additional steps for org-roam on Windows (optional)](#org5dd5877)
    2.  [Additional steps for latex on Windows (optional)](#org42de561)


<a id="orgd216df7"></a>

# Install Newbiemacs on Mac

1.  Download Emacs and install it. [Click here to download.](https://emacsformacosx.com/emacs-builds/Emacs-28.2-universal.dmg)
2.  Install git if it's not already installed. [Click here to download.](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
3.  Open a terminal and enter the following four lines. (You can open a
    terminal by pressing Command+Space and typing "terminal".)
    
        cd ~
        git clone https://github.com/jangsookim/newbiemacs.git
        mv newbiemacs nbm-root
        cp ~/nbm-root/.emacs ~/.emacs
4.  Run Emacs. It will take several minutes to download some
    packages. There may be some warnings but it's okay.
5.  Close Emacs and run it again. The above Newbiemacs main screeen will be shown.


<a id="org7fddf1f"></a>

## Additional steps for latex on Mac (optional)

If you want to edit tex files in Newbiemacs, you need to do the following (only once).

1.  Download skim. [Click here](https://skim-app.sourceforge.io/)
2.  Open skim.
3.  Press CMD+, (Preference set-up)
4.  Check the two check boxes.
5.  Select Emacs in Preset drop-down box.


<a id="org60bb9e3"></a>

# Install Newbiemacs on Windows

1.  Download Emacs and install it. [Click here](http://ftp.gnu.org/gnu/emacs/windows/emacs-28/emacs-28.2-installer.exe)
2.  Go to the directory "C:\Program Files\Emacs\emacs-28.2\bin" and create a shortcut for the file "runemacs.exe".
3.  Install git. [Click here](https://git-scm.com/download/win)
4.  Open "Command Prompt" (not "terminal") and enter the following
    lines.
    
        cd %userprofile%
        git clone https://github.com/jangsookim/newbiemacs.git
        rename newbiemacs nbm-root
        copy %userprofile%\nbm-root\.emacs-windows %userprofile%\AppData\Roaming\.emacs
5.  Run Emacs. It will take several minutes to download some
    packages. There may be some warnings but it's okay.
6.  Close Emacs and run it again. The above Newbiemacs main screeen will be shown.


<a id="org5dd5877"></a>

## Additional steps for org-roam on Windows (optional)

If you want to use org-roam, you need to do the following (only once). 

1.  Install perl. [Click here](https://strawberryperl.com/)
2.  Run Emacs and type the following. (SPC means the space key and RET means the return key (or the enter key).)
    
        SPC SPC package-refresh-contents RET
        SPC SPC package-install RET org-roam RET
        SPC SPC package-install RET org-roam-ui RET
3.  Restart Emacs. (It will take some time to compile EmacSQL SQLite binary.)


<a id="org42de561"></a>

## Additional steps for latex on Windows (optional)

If you want to use latex, you need to do the following (only once). 

1.  Install perl. (This can be omitted if you did this already) [Click here](https://strawberryperl.com/)
2.  Install msys2. (You have to remember the path to the directory of msys2. The default path is `C:\msys64`.) [Click here](https://www.msys2.org/)
3.  Run Emacs and type the following. (SPC means the space key and RET means the return key (or the enter key).)
    
        SPC SPC package-refresh-contents RET
        SPC SPC package-install RET pdf-tools RET
4.  Restart Emacs. Then the following message will be shown in the
    prompt: "Need to (re)build the epdfinfo program, do it now? (y or
    n)"
5.  Type y and then it will say "Do you have Msys2 installed? (y or n)"
6.  Type y again and it will ask you to enter the directory where Msys2 is installed.
7.  Enter the path that msys2 is installed. (The default one is `C:\msys64`.)
8.  On the prompt it will ask two questions. Type Enter and then Y. (This takes several minutes.)
9.  Restart Emacs.


