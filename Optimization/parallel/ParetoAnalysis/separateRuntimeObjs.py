import os

maindir = os.getcwd()
runtimeObjsList = [f for f in os.listdir(maindir + "/objs/runtime") if f[-4::] == '.obj']
for j in range(len(runtimeObjsList)):
    os.chdir(maindir + "/objs/runtime")
    with open(runtimeObjsList[j], mode="r") as bigfile:
        reader = bigfile.read()
        os.mkdir(maindir + "/objs/runtime/" + runtimeObjsList[j][:-4])
        os.chdir(maindir + "/objs/runtime/" + runtimeObjsList[j][:-4])
        for k,part in enumerate(reader.split("#")):
            with open(runtimeObjsList[j][:-4] + "_step" + str(int(k)) + ".obj", mode="w") as newfile:
                newfile.write(part + "#")