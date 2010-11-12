# -= Clean =-

rm -R ../../bin/Chess4Net_Skype

# -= Compilation =-

lazbuild ./Chess4Net_Skype_gtk2.lpi

# -= Build =-

mkdir ../../bin/_Chess4Net_Skype
mv ../../bin/Chess4Net_Skype ../../bin/_Chess4Net_Skype/
mv ../../bin/_Chess4Net_Skype ../../bin/Chess4Net_Skype
cp ../../Build/* ../../bin/Chess4Net_Skype/
cp ../../FPC/Lang.ini ../../bin/Chess4Net_Skype
#cp ../../Readme.txt ../../bin/Chess4Net_Skype/README
iconv -f CP1251 -t UTF8 ../../Readme.txt -o ../../bin/Chess4Net_Skype/README
cp ../../Readme_RU.txt ../../bin/Chess4Net_Skype/README_RU
#cp ../../FAQ.txt ../../bin/Chess4Net_Skype/FAQ
iconv -f CP1251 -t UTF8 ../../FAQ.txt -o ../../bin/Chess4Net_Skype/FAQ