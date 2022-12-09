# Newbiemacs

Newbiemacs is designed for mathematicians who are new to Emacs.

<img width="761" alt="스크린샷 2022-12-09 오전 9 08 10" src="https://user-images.githubusercontent.com/24665391/206592642-764fc8db-ae97-4b63-a6c9-0cee7e21ca15.png">



# Table of Contents

1.  [Install Newbiemacs on Mac](#orgc1dd8f0)
    1.  [Additional steps for latex on Mac (optional)](#orgf5a84eb)
2.  [Install Newbiemacs on Windows](#orgcf71f8b)
    1.  [Additional steps for org-roam on Windows (optional)](#org1a34072)
    2.  [Additional steps for latex on Windows (optional)](#orgb48025c)


<a id="orgc1dd8f0"></a>

# Install Newbiemacs on Mac

1.  Download Emacs and install it. [Click here to download.](https://emacsformacosx.com/emacs-builds/Emacs-28.2-universal.dmg)
2.  Install git if it's not already installed. [Click here to download.](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
3.  Open a terminal and enter the following lines. (You can open a
    terminal by pressing Command+Space and typing "terminal".)
    
        cd ~
        git clone https://github.com/jangsookim/newbiemacs.git
        mv newbiemacs nbm-root
        cp ~/nbm-root/.emacs ~/.emacs
4.  Run Emacs. It will take several minutes to download some
    packages. There may be some warnings but it's okay.
5.  Close Emacs and run it again. The above Newbiemacs main screeen will be shown.


<a id="orgf5a84eb"></a>

## Additional steps for latex on Mac (optional)

If you want to edit tex files in Newbiemacs, you need to do the following (only once).

1.  Install Skim. [Click here](https://skim-app.sourceforge.io/)
2.  Open skim.
3.  Press CMD+, (Preference set-up)
4.  Check the two check boxes.
5.  Select Emacs in Preset drop-down box.


<a id="orgcf71f8b"></a>

# Install Newbiemacs on Windows

1.  Download Emacs and install it. [Click here](http://ftp.jaist.ac.jp/pub/GNU/emacs/windows/emacs-28/) and download "emacs-28.2-installer.exe".
2.  Install git. [Click here](https://git-scm.com/download/win)
3.  Open "Command Prompt" (not "terminal") and enter the following
    lines.
    
        cd %userprofile%
        git clone https://github.com/jangsookim/newbiemacs.git
        rename newbiemacs nbm-root
        copy %userprofile%\nbm-root\.emacs-windows %userprofile%\AppData\Roaming\.emacs
4.  Run Emacs. It will take several minutes to download some
    packages. There may be some warnings but it's okay.
5.  Close Emacs and run it again. The above Newbiemacs main screeen will be shown.


<a id="org1a34072"></a>

## Additional steps for org-roam on Windows (optional)

If you want to use org-roam, you need to do the following (only once). 

1.  Install Perl. [Click here](https://strawberryperl.com/)
2.  Install [Google Chrome](https://www.google.com/chrome/) if it's not installed. (Needed to view node connections.)
3.  Run Emacs and type the following. (SPC means the space key and RET
    means the return key (or the enter key). There may be some warnings but it's okay.)
    
        SPC SPC package-refresh-contents RET
        SPC SPC package-install RET org-roam RET
        SPC SPC package-install RET org-roam-ui RET
4.  Restart Emacs. (It will take some time to compile EmacSQL SQLite binary.)


<a id="orgb48025c"></a>

## Additional steps for latex on Windows (optional)

If you want to use latex, you need to do the following (only once). 

1.  Install Perl. (This can be omitted if it's already installed.) [Click here](https://strawberryperl.com/)
2.  Install Sumatra PDF. [Click here](https://www.sumatrapdfreader.org/free-pdf-reader)
3.  Open Sumatra PDF and go to settings and click Sumatra PDF option.
4.  Enter the following line in the inverse search setting.
    
        C:\Program Files\Emacs\emacs-28.2\bin\emacsclientw.exe -n +%l "%f"


