# huffman
A toy implementation of Huffman file compression (and a foray into Scala) 

Uses Maven with [scala-maven-plugin] (http://davidb.github.io/scala-maven-plugin/) 
so it may be run without installing Scala.

##Usage
```
amar@localhost:~/huffman$ mvn scala:run

Usage: huffman [OPTION] INPUT_FILE OUTPUT_FILE
Compress or decompress INPUT_FILE and write to OUTPUT_FILE

 Options:
    -c               Compress
    -d               Decompress
```
####Compress
```
amar@localhost:~/huffman$ mvn scala:run -DaddArgs="-c|src/main/resources/alice29.txt|compressed.huf"

      Uncompressed size: 148480 bytes
        Compressed size: 90626 bytes
          Space savings: 38.96%
```
####Decompress
```
amar@localhost:~/huffman$ mvn scala:run -DaddArgs="-d|compressed.huf|alice.txt"

amar@localhost:~/huffman$ diff -s alice.txt src/main/resources/alice29.txt

Files alice.txt and src/main/resources/alice29.txt are identical
```
