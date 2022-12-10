# Newbiemacs

Newbiemacs is designed for mathematicians who are new to Emacs.

<img width="761" alt="스크린샷 2022-12-09 오전 9 08 10" src="https://user-images.githubusercontent.com/24665391/206592642-764fc8db-ae97-4b63-a6c9-0cee7e21ca15.png">



# Table of Contents

1.  [Install Newbiemacs on Mac](#orgad8456a)
    1.  [Additional steps for latex on Mac (optional)](#org435d3d6)
2.  [Install Newbiemacs on Windows](#org0f2388d)
    1.  [Additional steps for org-roam on Windows (optional)](#org8dc06a2)
    2.  [Additional steps for latex on Windows (optional)](#orgadec0fc)


<a id="orgad8456a"></a>

# Install Newbiemacs on Mac

1.  Download Emacs and install it. [Click here](https://emacsformacosx.com/)
2.  Install Git if it's not installed. [Click here](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
3.  Open a terminal and enter the following lines. (You can open a
    terminal by pressing Command+Space and typing "terminal". You can
    just copy the whole thing by clicking the icon on the top-right
    corner of the box below and then paste it in your terminal.)
    
        cd ~
        git clone https://github.com/jangsookim/newbiemacs.git
        mv newbiemacs nbm-root
        cp ~/nbm-root/.emacs ~/.emacs
4.  Run Emacs. It will take several minutes to download necessary
    packages. There may be some warnings but it's okay.
5.  Close Emacs and run it again. The above Newbiemacs main screeen will be shown.


<a id="org435d3d6"></a>

## Additional steps for latex on Mac (optional)

If you want to edit tex files in Newbiemacs, you need to do the following (only once).

1.  Install Skim. [Click here](https://skim-app.sourceforge.io/)
2.  Install MacTeX (this is TeX Live for Mac) if it's not installed. [Click here](https://www.tug.org/mactex/)
3.  Open Skim and press `CMD+,` (Hold the command key and press the comma key.)
4.  Go to the `Sync` menu and check the two check boxes.
5.  Select Emacs in the drop-down box for `Preset`.


<a id="org0f2388d"></a>

# Install Newbiemacs on Windows

1.  Download Emacs and install it. [Click here](http://ftp.jaist.ac.jp/pub/GNU/emacs/windows/emacs-28/) and download "emacs-28.2-installer.exe".
2.  Install Git if it's not installed. [Click here](https://git-scm.com/download/win) (Download "Standalone Installer".)
3.  Open "Command Prompt" (not "terminal" or "PowerShell") and enter the following
    lines. (You can just copy the whole thing by clicking the icon on
    the top-right corner of the box below and then paste it in your
    command prompt.)
    
        cd %userprofile%
        git clone https://github.com/jangsookim/newbiemacs.git
        rename newbiemacs nbm-root
        copy %userprofile%\nbm-root\.emacs-windows %userprofile%\AppData\Roaming\.emacs
4.  Run Emacs. It will take several minutes to download necessary packages. There
    may be some warnings but it's okay.
5.  Close Emacs and run it again. The above Newbiemacs main screeen will be shown.


<a id="org8dc06a2"></a>

## Additional steps for org-roam on Windows (optional)

If you want to use org-roam, you need to do the following (only once). 

1.  Install Perl if it's not installed. [Click here](https://strawberryperl.com/)
2.  Install [Google Chrome](https://www.google.com/chrome/) if it's not installed. (Needed to view node connections.)
3.  Run Emacs and type the following. (SPC means the space key and RET
    means the return key, i.e., the enter key. There may be some warnings but it's okay.)
    
        SPC SPC package-refresh-contents RET
        SPC SPC package-install RET org-roam RET
        SPC SPC package-install RET org-roam-ui RET
4.  Restart Emacs. (It will take some time to compile EmacSQL SQLite binary.)


<a id="orgadec0fc"></a>

## Additional steps for latex on Windows (optional)

If you want to use latex, you need to do the following (only once). 

1.  Install Perl if it's not installed. [Click here](https://strawberryperl.com/)
2.  Install TeX Live if it's not installed. [Click here](https://www.tug.org/texlive/windows.html) (It may take about 2 hours.)
3.  Install Sumatra PDF. [Click here](https://www.sumatrapdfreader.org/free-pdf-reader) (Download "Installer", not "Portable version".)
4.  Open Sumatra PDF and in the menu go to `Settings` and click `Options...`.
5.  Enter the following line in the box below `Set inverse search command-line`.
    
        C:\Program Files\Emacs\emacs-28.2\bin\emacsclientw.exe -n +%l "%f"
    
    If the box is not visible, then in the menu go to `Settings` and click `Advanced Options...`.
    It will open a text file. Enter the following line anywhere there and save the file.
    
        InverseSearchCmdLine = C:\Program Files\Emacs\emacs-28.2\bin\emacsclientw.exe -n +%l "%f"

