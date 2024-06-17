import os

maindir = os.getcwd()
runtimeObjsList = [f for f in os.listdir(maindir + "/runtime") if f[-8::] == '.runtime']
for j in range(len(runtimeObjsList)):
    os.chdir(maindir + "/runtime")
    with open(runtimeObjsList[j], mode="r") as bigfile:
        reader = bigfile.read()
        os.mkdir(maindir + "/runtime/" + runtimeObjsList[j][:-8])
        os.chdir(maindir + "/runtime/" + runtimeObjsList[j][:-8])
        for k,part in enumerate(reader.split("#")):
            with open(runtimeObjsList[j][:-8] + "_step" + str(int(k)) + ".set", mode="w") as newfile:
                newfile.write(part + "#")