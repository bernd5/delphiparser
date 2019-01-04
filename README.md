# delphi

Very simple for now.

This is intended to be just a Delphi parser to an AST, including comments.

Not all functionality is implemented yet.

# Manual testing

In addition to automated testing, manual testing is done by parsing PyScripter and the free pascal project sources.
 - Zero errors are expected.

# Difficult cases

### Compiler Directives.
1. Some comments are compiler directives. {+M}, as an example.  These are listed for Embarcadero at http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/devcommon/delphicompdirectivespart_xml.html  The following link also hints as to special handling of Compiler Directives: http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Fundamental_Syntactic_Elements_(Delphi)#Comments_and_Compiler_Directives
2. Some compiler directives are compiler specific.  Eg, FPC has it's own list: https://www.freepascal.org/docs-html/prog/progch1.html

Compiler directives are handled explicitly as a CompilerDirective node, these, however, can be placed wherever a comment can be placed, so CompilerDirective Nodes mimic the Comment nodes.

Parsing them will probably require looking ahead and getting the shortest working parse, then rewinding and trying the other options, and using catMaybes to get a list of nodes that parse.

### Extended discussion

 First, let us consider the preprocessor (that is, compiler directives).

Consider the following:

> TDeallocator = {$IFNDEF FPC}reference to{$ENDIF} procedure(AObj: TObject);

There are a few options when parsing this:

1. Could ignore all comments, including the preprocessor defines.
2. Could consider the preprocessor defines as a valid part of the AST, in all it's glory.
3. Evaluate the preprocessor, and provide the various versions that the AST can contain as a result of each branch.
4. Could ignore all comments, including the preprocessor defines, AND include the original text as a property of the node.

The first option is obviously the simplest, however, if the AST is used as a code generator, this means that comments can not be included, which is quite limiting.  It means that this AST can not be used as a code preprocessor, therefore, I do not like this option.

In Delphi, and in FPC, two major compilers, comments can't actually go anywhere, they can only go in specific places (ie, not in the middle of a word).  So option 2 - consiering all preprocessor defines (and by extension, all comments) as a regular part of the AST is a feasible solution.
 There is one difficulty, however, with some preprocessor directives that effectively insert or remove code, which influences the AST.
 - These will have to be somehow catered for in the AST.

The third option, to evaluate the preprocessor, and provide the various versions as a result, is the most elegant solution for me.  It does mean implementing the preprocessors, and it does introduce a more complex AST, however, this can still result in an AST that closely represents the original text (even if the preprocessor expressions become simplified as a result).  It might not require a complete impelmentation of a preprocessor, and it may be enough to simply evaluate the branches on the fly.

The fourth option is also a possible option - but is likely to result in unparseable code, so is not a suitable option.

General source comments are another challenge.  Like compiler directives, they could be included literally anywhere.  The simplest approach is to consider a few conventions:

1. If a comment includes anywhere within a parseable node, it becomes attached to that node. Code generation can show the comment after the node.
2. Some comments occur on their own line, these should be attached to the following node, however, code generation will want to show these BEFORE the node.

As an example of the two different usages of comments, consider the following:

>  { Here be dragons }
>  procedure EnterTheLair;
>  begin
>    BreatheFire; // The dragon in the lair is breathing fire.
>  end;

A final consideration, is to perhaps store the original text representing the node - comments and all - and to emit it during code generation if there are no preprocessor commands, and the comments are in unusual spots.

With all that in mind, the comment strategy is likely to consist of:

1. Syntatically significant comments (GUID's, compiler options, etc), to be explicitly catered for as such in the code, as these aren't really comments, and
2. All other comments should be tagged as a 'comment' node, of the relvant type given their context.

