# Newbiemacs

Newbiemacs is designed for mathematicians who are new to Emacs.

<img width="761" alt="스크린샷 2022-12-09 오전 9 08 10" src="https://user-images.githubusercontent.com/24665391/206592642-764fc8db-ae97-4b63-a6c9-0cee7e21ca15.png">


## Install Newbiemacs on Mac
1. Download Emacs and install it. https://emacsformacosx.com/emacs-builds/Emacs-28.2-universal.dmg
2. Install git if it's not already installed. https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
3. Open a terminal and enter the following four lines. (You can open a
   terminal by pressing Command+Space and typing "terminal".)
   ```
   cd ~
   git clone https://github.com/jangsookim/newbiemacs.git
   mv newbiemacs nbm-root
   cp ~/nbm-root/.emacs ~/.emacs
   ```
4. Run Emacs. It will take several minutes to download some packages.
5. There may be some warnings but it's okay.
6. Close Emacs and run it again.
## Additional programs for Mac
### skim (pdf viewer in mac)
1. Download skim https://skim-app.sourceforge.io/
2. Open skim.
3. Press CMD+, (Preference set-up)
4. Check the two check boxes.
5. Select Emacs in Preset drop-down box.
## Install Newbiemacs on Windows
1. Download Emacs and install it. http://ftp.gnu.org/gnu/emacs/windows/emacs-28/emacs-28.2-installer.exe
2. Go to the directory "C:\Program Files\Emacs\emacs-28.2\bin" and create a shortcut for the file "runemacs.exe".
3. Install git. https://git-scm.com/download/win
4. Open a terminal and enter the following lines.
   ```
   cd %userprofile%
   git clone https://github.com/jangsookim/newbiemacs.git
   rename newbiemacs nbm-root
   copy %userprofile%\nbm-root\.emacs-windows %userprofile%\AppData\Roaming\.emacs
   ```
5. Run Emacs. It will take several minutes to download some packages.
6. There may be some warnings but it's okay.
7. Close Emacs and run it again.
## Additional programs for Windows
### msys2 (needed for pdf-tools)
https://www.msys2.org/
### perl (needed for latexmk)
https://strawberryperl.com/
