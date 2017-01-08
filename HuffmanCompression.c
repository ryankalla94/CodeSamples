#include <stdio.h>
#include <string.h>
#include <ctype.h> 
#include <stdlib.h>
#include <math.h>

#define TRUE 1
#define MAX_FILENAME_LENGTH 100
#define MAX_FILESIZE 1000000
#define MAX_NUM_SYMBOLS 255
#define MAX_CODE_SIZE 50
#define MAX_BITSTRING_LENGTH 255


// An implementation of Huffman Coding used to Compress and Decompress text files 


// represents a node in the Huffman tree
struct node {
    char symbol;
    int weight;
    struct node *left;
    struct node *right;
};

// a linked list of symbols and their binary codes
struct code {
    char symbol;
    char binary[MAX_CODE_SIZE];
    struct code *next;
};

// gets the character weights of the input file
void getWeights(char file_buffer[MAX_FILESIZE], int weights[MAX_NUM_SYMBOLS]){
    for (int i = 0; i < strlen(file_buffer); i++){
        weights[file_buffer[i]]++;
    }
}

// makes a Huffman tree node
struct node makeNode(struct node *l, struct node *r){
    struct node newNode = {
        .weight = l->weight + r->weight,
        .left = l,
        .right = r,
    };
    return newNode;
}

// finds the smallest node in a node array
struct node findSmallest(struct node nodeArray[], int size){
    struct node currentSmallest = nodeArray[0];
    for (int i = 0; i < size; i++){
        if(nodeArray[i].weight < currentSmallest.weight){
            currentSmallest = nodeArray[i];
        }
    }
    return currentSmallest;
}

// removes the smallest node from a node array
void removeSmallest(struct node nodeArray[], int size, struct node newArray[size-1]){
    struct node currentSmallest = nodeArray[0];
    for(int i = 1; i < size; i++){
        if(nodeArray[i].weight < currentSmallest.weight){
            newArray[i-1] = currentSmallest;
            currentSmallest = nodeArray[i];
        } else{
            newArray[i-1] = nodeArray[i];
        }
    }
}

// prints the Huffman tree (used for testing)
void printNode(struct node *n){
    if(n == NULL){
        return;
    }
    printf("(");
    printNode(n->left);
    if(n->left == NULL && n->right == NULL){
        printf("(%c %d)",n->symbol, n->weight);
    } else {
        printf("(%d)", n->weight);
    }
    printNode(n->right);
    printf(")");
}

// constructs the Huffman tree
struct node makeTree(struct node nodeArray[], int size, struct node newArray[size-1]){
    if(size == 1){
        return nodeArray[0];
    } else{
        struct node* smallest = malloc(sizeof(struct node));
        struct node temp1 = findSmallest(nodeArray, size);
        smallest->symbol = temp1.symbol;
        smallest->weight = temp1.weight;
        smallest->left = temp1.left;
        smallest->right = temp1.right;
        struct node step1Array[size-1];
        removeSmallest(nodeArray, size, step1Array);
        struct node* secondSmallest = malloc(sizeof(struct node));
        struct node temp2 = findSmallest(step1Array, size-1);
        secondSmallest->symbol = temp2.symbol;
        secondSmallest->weight = temp2.weight;
        secondSmallest->left = temp2.left;
        secondSmallest->right = temp2.right;
        struct node step2Array[size-2];
        removeSmallest(step1Array, size-1, step2Array);
        struct node newNode = makeNode(smallest, secondSmallest);
        for(int i = 0; i < size-2; i++){
            newArray[i] = step2Array[i];
        }
        newArray[size-2] = newNode;
        struct node newArray2[size-2];
        return makeTree(newArray, size-1, newArray2);
    }
}

// determines the size of the Huffman tree (number of leaves)
int treeSize(struct node *tree){
    if(tree->right == NULL && tree->left == NULL){
        return 1;
    } else{
        return treeSize(tree->left) + treeSize(tree->right);
    }
}

// combines two code lists
struct code *combineCodes(struct code *code1, struct code *code2){
    if(code1->next == NULL){
        code1->next = code2;
    } else{
        code1->next = combineCodes(code1->next, code2); 
    }
    return code1;
}

// determines the variable sized binary code for each unique symbol in the Huffman tree
struct code *getCodes(struct node *tree, char binaryCode[MAX_CODE_SIZE]){
    if(tree->left == NULL && tree->right == NULL){
        struct code* newCode = malloc(sizeof(struct code));
        newCode->symbol = tree->symbol;
        for(int i = 0; i < MAX_CODE_SIZE; i++){
            newCode->binary[i] = binaryCode[i];
        }
        newCode->next = NULL;
        return newCode;
    } else{
        char leftCode[MAX_CODE_SIZE];
        char rightCode[MAX_CODE_SIZE];
        for(int i = 0; i < MAX_CODE_SIZE; i++){
            leftCode[i] = binaryCode[i];
            rightCode[i] = binaryCode[i];
        }
        int length = strlen(binaryCode);
        leftCode[length] = '0';
        leftCode[length+1] = '\0';
        rightCode[length] = '1';
        rightCode[length+1] = '\0';
        return combineCodes(getCodes(tree->left, leftCode),
                            getCodes(tree->right, rightCode));
    }
}

// prints the list of binary codes (used for testing)
void printCode(struct code *c){
    if(c != NULL){
        printf("%c %s\n", c->symbol, c->binary);
        printCode(c->next);
    }

}

// writing bits to a file, code from class;
char writeCode(FILE *f, struct code *myCode, int start, char c){
    int bitsToWrite = strlen(myCode->binary);
    for(int i = 0; i < bitsToWrite; i++){
        int bit_position = (i + start) % 8;
        if(myCode->binary[i] == '1'){
            char bit_mask = 1 << (7 - bit_position);
            c |= bit_mask;
        }
        if(bit_position == 7){
            fprintf(f, "%c", c);
            c = 0;
        } 
    }
    return c;
}

// finds a specific code given a character
struct code *findCode(char c, struct code *myCode){
    if(myCode == NULL){
        printf("code not found - %c\n", c);
        return myCode;
    }
    if(myCode->symbol == c){
        return myCode;
    } else{
        return findCode(c, myCode->next);
    }
}

// converts a string representation of a number into an integer
int stringToInt(char string[]){
    int length = strlen(string);
    int num = 0;
    for(int i = 0; i < length; i++){
        num = (10 * num) + string[i] - 48;
    }
    return num;
}

// determines if two string are equal
int stringEquals(char s1[MAX_CODE_SIZE], char s2[MAX_CODE_SIZE]){
    int length1 = strlen(s1);
    int length2 = strlen(s2);
    if(length1 == length2){
        int b = 1;
        for(int i = 0; i < length1; i ++){
            if(s1[i] != s2[i]){
                b = 0;
            }
        }
        return b;
    } else{
        return 0;
    }
}

// determines if a code matches any of the codes from the Huffman tree
int matchingCodes(char binary[], struct code *myCode){
    int b = 0;
    for(struct code *c = myCode; c!=NULL; c = c->next){
        if(stringEquals(binary, c->binary)){
            b = 1;
        }
    }
    return b;
}

// returns the specific character given a binary code
char findChar(char binary[], struct code *myCode){
    if(myCode == NULL){
        printf("char not found \n");
        return 0;
    }
    if(stringEquals(binary, myCode->binary)){
        return myCode->symbol;
    } else{
        return findChar(binary, myCode->next);
    }
}

// determines if a file is of the correct type to be decompressed
int checkFileType(char filename[]){
    int length = strlen(filename);
    if(length > 5){
    return filename[length-5] == '.' &&
           filename[length-4] == 'c' &&
           filename[length-3] == 'o' &&
           filename[length-2] == 'm' &&
           filename[length-1] == 'p';
    } else{
        return 0;
    }
    
}


int main() {
    char initialResponse[MAX_FILENAME_LENGTH];
    printf("Enter 'c' to compress a file, or 'd' to decompress a file: ");
    fgets(initialResponse, MAX_FILENAME_LENGTH, stdin);
    if(initialResponse[0] == 'c'){
        // Compression
        // Get the input file --- code from lecture
        char filename[MAX_FILENAME_LENGTH];
        printf("Enter name of file to compress: ");
        fgets(filename, MAX_FILENAME_LENGTH, stdin);
        printf("Compressing...\n");
        /* Trim newline if necessary */
        int len = strlen(filename);
        if (filename[len-1] == '\n') {
            filename[len-1] = '\0';
        }
        FILE *file_in;
        file_in = fopen(filename, "r");  /* Open for reading */
        if (file_in == NULL) {
            printf("No file '%s' found!\n", filename);
        }
        char file_buffer[MAX_FILESIZE];
        int index = 0;
        while (TRUE) {
            if (feof(file_in)) {  /* End of file */
                file_buffer[index] = '\0';
                break;
            } else {
                file_buffer[index] = fgetc(file_in);
                index++;
            }
        }
        fclose(file_in);
        int weights[MAX_NUM_SYMBOLS];
        // initializing weights to 0;
        for(int i = 0; i < MAX_NUM_SYMBOLS; i++){
            weights[i] = 0;
        }
        getWeights(file_buffer, weights);
        // number of symbols that have a positive weight
        int uniqueNumSymbols;
        for (int i = 0; i < MAX_NUM_SYMBOLS; i++) {
            if (weights[i] > 0){
                uniqueNumSymbols++;
            }
        }
        // create an array of single node trees
        struct node nodeArray[uniqueNumSymbols];
        int nodeArrayIndex = 0;
        // each node is a unique symbol
        for(int i = 0; i < MAX_NUM_SYMBOLS; i++){
            if (weights[i] > 0){
                struct node new = {
                    .symbol = i,
                    .weight = weights[i],
                    .left = NULL,
                    .right = NULL,
                };
                nodeArray[nodeArrayIndex] = new;
                nodeArrayIndex++;
            }
        }
        // convert the node array into a Huffman Tree
        struct node otherArray[uniqueNumSymbols-1];
        struct node tree = makeTree(nodeArray, uniqueNumSymbols, otherArray);
        char binaryCodeArray[MAX_CODE_SIZE];
        binaryCodeArray[0] = '\0';
        int numSymbols = strlen(file_buffer);
        int file_length = index;
        // create output file with correct extension
        char outputFilename[MAX_FILENAME_LENGTH+5];
        for(int i = 0; i < MAX_FILENAME_LENGTH; i++){
            outputFilename[i] = filename[i];
        }
        int filenameLen = strlen(filename);
        outputFilename[filenameLen] = '.';
        outputFilename[filenameLen+1] = 'c';
        outputFilename[filenameLen+2] = 'o';
        outputFilename[filenameLen+3] = 'm';
        outputFilename[filenameLen+4] = 'p';
        outputFilename[filenameLen+5] = '\0';
        FILE *file_out = fopen(outputFilename, "w");
        // print the number of unique symbols, and the total number of symbols
        // (used for decompression)
        fprintf(file_out, "%d\n", uniqueNumSymbols);
        fprintf(file_out, "%lu\n", strlen(file_buffer)-1);
        // get the binary codes for output
        struct code *myBinaryCodes = getCodes(&tree, binaryCodeArray);
        // print the binary codes at the start of the file (used for decompression)
        for(struct code *c = myBinaryCodes; c!=NULL; c= c->next){
            fprintf(file_out, "%c%s\n", c->symbol, c->binary);
        }
        //write the binary to the output file
        char c = 0;
        int start = 0;
        for(int i = 0; i < numSymbols-1; i++){
            struct code *myCode = findCode(file_buffer[i], getCodes(&tree, binaryCodeArray));
            c = writeCode(file_out, myCode, start, c);
            start = (start + strlen(myCode->binary)) % 8;
        }
        fprintf(file_out, "%c", c);
        fclose(file_out);
        printf("File written to %s\n", outputFilename);
    } else if(initialResponse[0] == 'd'){
        // Decompression
        // Get the input file --- code from lecture
        char filename[MAX_FILENAME_LENGTH];
        printf("Enter name of file to decompress: ");
        fgets(filename, MAX_FILENAME_LENGTH, stdin);
        /* Trim newline if necessary */
        int len = strlen(filename);
        if (filename[len-1] == '\n') {
            filename[len-1] = '\0';
        }
        // check that the file is of the right type
        if(checkFileType(filename)){
            printf("Decompressing...\n");
            FILE *file_in;
            file_in = fopen(filename, "r");  /* Open for reading */
            if (file_in == NULL) {
                printf("No file '%s' found!\n", filename);
            }
            char file_buffer[MAX_FILESIZE];
            int index = 0;
            while (TRUE) {
                if (feof(file_in)) {  /* End of file */
                    file_buffer[index] = '\0';
                    break;
                } else {
                    file_buffer[index] = fgetc(file_in);
                    index++;
                }
            }
            fclose(file_in);
            char uniqueNumSymbolsString[MAX_CODE_SIZE];
            char totalNumSymbolsString[MAX_CODE_SIZE];
            index = 0;
            // get the number of unique symbols
            while(file_buffer[index] != '\n'){
                uniqueNumSymbolsString[index] = file_buffer[index];
                index++;
            }
            uniqueNumSymbolsString[index] = '\0';
            index++;
            int index2 = 0;
            // get the total number of symbols
            while(file_buffer[index] != '\n'){
                totalNumSymbolsString[index2] = file_buffer[index];
                index++;
                index2++;;
            }
            totalNumSymbolsString[index2] = '\0';
            index++;
            index2 = 0;
            int uniqueNumSymbols = stringToInt(uniqueNumSymbolsString);
            int totalNumSymbols = stringToInt(totalNumSymbolsString);
            struct code* myCode = malloc(sizeof(struct code));
            myCode->symbol = file_buffer[index];
            index++;
            char myBinaryCode[MAX_CODE_SIZE];
            // get the binary codes
            while(file_buffer[index] != '\n'){
                myBinaryCode[index2] = file_buffer[index];
                index++;
                index2++;;
            }
            myBinaryCode[index2] = '\0';
            for(int i = 0; i < MAX_CODE_SIZE; i++){
                myCode->binary[i] = myBinaryCode[i];
            }
            index++;
            index2 = 0;
            myCode->next = NULL;
            for(int i = 0; i < uniqueNumSymbols-1; i++){
                struct code* myCode2 = malloc(sizeof(struct code));
                myCode2->symbol = file_buffer[index];
                index++;
                while(file_buffer[index] != '\n'){
                    myBinaryCode[index2] = file_buffer[index];
                    index++;
                    index2++;;
                }
                myBinaryCode[index2] = '\0';
                for(int i = 0; i < MAX_CODE_SIZE; i++){
                    myCode2->binary[i] = myBinaryCode[i];
                }
                index++;
                index2 = 0;
                myCode2->next = myCode;
                myCode = myCode2;
            }
            int filenameLen = strlen(filename);
            // truncate the filename
            filename[filenameLen-5] = '\0';
            FILE *file_out = fopen(filename, "w");
            // convert codes back into characters
            char currentCode[MAX_CODE_SIZE];
            char c = 0;
            int bit_index = 0;
            for(int i = 0; i < totalNumSymbols; i++){
                int codeIndex = 0;
                currentCode[codeIndex] = '\0';
                while(!matchingCodes(currentCode, myCode)){
                    int bit_position = bit_index % 8;
                    int mask = 1 << (7 - bit_position);
                    char fileChar = file_buffer[index];
                    mask &= fileChar;
                    if(mask != 0){
                        currentCode[codeIndex] = '1';
                    } else{
                        currentCode[codeIndex] = '0';
                    }
                    codeIndex++;
                    currentCode[codeIndex] = '\0';

                    if(bit_position == 7){
                        index++;
                    }
                    bit_index++;
                }
                c = findChar(currentCode, myCode);
                fprintf(file_out, "%c", c); // when a code matches, print the char
            }
            fclose(file_out);
            printf("File written to %s\n", filename);
        } else{
            printf("Sorry, wrong file type\n");
        }
    } else{
        printf("Sorry, command not recognized\n");
    }

    return 0;
}