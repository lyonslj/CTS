import os
import subprocess

def extract_files(directory):
  for file in os.listdir(directory):
     if file.endswith(".lzh") and os.path.getsize(os.path.join(directory, file)) > 1024:
      subprocess.call(["/Applications/The Unarchiver.app", file])

if __name__ == "__main__":
  directory = "/Users/johnlyons/Downloads"
  extract_files(directory)
