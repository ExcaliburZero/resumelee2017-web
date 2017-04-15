# Encryption-Decryption Program
This is a program that can encrypt and decrypt files into or from multiple pieces.

You can get the source code for this program at the following link. (I would provide an executable binary, but that would be a terrible secruity practice)

* [GitHub repo](https://github.com/ExcaliburZero/resumelee2017)

## Usage
First, make sure that you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed. Then run the following command in the main directory of the project to get the correct compiler version and compile the program.

```
$ stack setup
$ stack build
$ stack install
```

### Encrypting Files
First you will want to encrypt  the file using `gpg`, you can do this by running the following commands.

```
$ ls
resume.pdf

$ gpg -c resume.pdf

$ ls
resume.pdf  resume.pdf.gpg
```


To break up a file, run the following command, specifying the file to break up and number of pieces to split it into.

```
$ stack exec resumelee2017 -- encrypt FILE_NAME NUM_PIECES
```

For example, you can break up a file `resume.pdf` into 5 pieces by doing the following:

```
$ stack exec resumelee2017 -- encrypt resume.pdf.gpg 5
Encrypting file: resume.pdf.gpg

$ ls
resume.pdf            resume.pdf.gpg.part3
resume.pdf.gpg        resume.pdf.gpg.part4
resume.pdf.gpg.part1  resume.pdf.gpg.part5
resume.pdf.gpg.part2
```

### Decrypting Files
To decrypt a file, place all of the file pieces in the same directory and give the name of the decrypted file and the number of file pieces.

```
$ stack exec resumelee2017 -- decrypt FILE_NAME NUM_PIECES
```

For example, you can decrypt a file `resume.pdf` that is split into 5 pieces by doing the following:

```
$ ls
resume.pdf.gpg.part1  resume.pdf.gpg.part4
resume.pdf.gpg.part2  resume.pdf.gpg.part5
resume.pdf.gpg.part3

$ stack exec resumelee2017 -- decrypt resume.pdf.gpg 5
Decrypting file: resume.pdf.gpg

$ ls
resume.pdf.gpg        resume.pdf.gpg.part3
resume.pdf.gpg.part1  resume.pdf.gpg.part4
resume.pdf.gpg.part2  resume.pdf.gpg.part5
```

Once you have put the pieces back together, you will need to decrypt it using gpg.

```
$ gpg -d resume.pdf.gpg > resume.pdf

$ ls
resume.pdf            resume.pdf.gpg.part3
resume.pdf.gpg        resume.pdf.gpg.part4
resume.pdf.gpg.part1  resume.pdf.gpg.part5
resume.pdf.gpg.part2
```

## License
The source code of this program is available under the [MIT license](https://opensource.org/licenses/MIT).
