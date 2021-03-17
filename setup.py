# This program is a part of EASIFEM library.
# See. www.easifem.com
# Copyright (c) 2020-2021, All right reserved, Vikas Sharma, Ph.D.
#

import os
import sys
import platform

str=" _______     ___           _______. __   _______  _______ .___  ___. "
print(str)
str="|   ____|   /   \         /       ||  | |   ____||   ____||   \/   | "
print(str)
str="|  |__     /  ^  \       |   (----`|  | |  |__   |  |__   |  \  /  | "
print(str)
str="|   __|   /  /_\  \       \   \    |  | |   __|  |   __|  |  |\/|  | "
print(str)
str="|  |____ /  _____  \  .----)   |   |  | |  |     |  |____ |  |  |  |"
print(str)
str="|_______/__/     \__\ |_______/    |__| |__|     |_______||__|  |__|"
print(str+"\n")
str = "Expandable And Scalable Infrastrcture for Finite Element Methods"
print(str)
str = "Developed by Vikas Sharma, Ph. D."
print(str)
str = "(c) 2020-2021"
print(str)
print("================================================================\n")


print("Detecting OS type...")
_os = platform.system()
if _os == 'Windows':
    print("Windows platform found")
    print("Setting up for Windows...")
    #os.system("install.bat")
    print("Please use Windows Subsystem Linux(WSL) ")
    print("Installation DONE!!")
elif _os == "Darwin":
    print("MacOSX system found")
    print("Setting up for MacOSX...")
    os.system("sh ./setup/Darwin.sh")
    print("Installation DONE!!")
elif _os == "Linux":
    print("MacOSX system found")
    print("Setting up for MacOSX...")
    # os.system("sh ./setup/setup")
    print("Installation DONE!!")
else:
    print("ERROR: Unknown Operating System")
