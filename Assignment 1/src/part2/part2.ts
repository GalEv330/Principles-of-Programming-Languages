import * as R from "ramda";

const stringToArray = R.split("");

/* Question 1 */
export const countVowels: (str: string) => number = (str: string) => {
    return R.pipe(
        stringToArray,
        R.reduce((acc: number, curr: string) => (isVowel(curr) ? acc + 1 : acc), 0)
    )(str);
};
/* Helper function to check if a character is a vowel */
const isVowel = (ch: string): boolean => { 
    return ch === "a" || ch === "e" || ch === "i" || ch === "o" || ch === "u"
    || ch === "A" || ch === "E" || ch === "I" || ch === "O" || ch === "U";
};

/* Question 2 */
export const isPaired: (stack: string) => boolean = (str: string) => {
    return R.pipe(
        stringToArray,
        R.filter(isParenthesis),
        R.reduce(updateStack, [" "]),
        R.equals([" "])
    )(str);
};

/* Helper function to check if a character is a parenthesis */
const isParenthesis = (ch: string): boolean => {
    return ch === "{" || ch === "}" || ch === "(" || ch === ")" || ch === "[" || ch === "]";
};

/* Helper function to check if closing parentheses matches the last found opening parentheses */
const isMatchingPair  = (open: string[], close: string): boolean => {
    return (R.last(open) === "{" && close === "}") || 
           (R.last(open) === "(" && close === ")") || 
           (R.last(open) === "[" && close === "]");
};

/* Helper function that adds the current character to the stack if it is an opening parenthesis
   and removes the last element if the current character is a matching closing parenthesis */
const updateStack = (stack: string[], ch: string): string[] => {
    return (isMatchingPair(stack, ch) ? R.init(stack) : R.append(ch, stack));
};

/* Question 3 */
export type WordTree = {
    root: string;
    children: WordTree[];
}

export const treeToSentence : (t: WordTree) => string = (t: WordTree) => {
    if (t.children.length === 0) return t.root;
    return t.root + " " + t.children.map(treeToSentence).join(" ");
};
