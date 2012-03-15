package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.LANG_FSHARP;

public class FSharpScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, BLANK),   "\n");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, BLANK),   "     \n");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, BLANK),   "\t\n");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, CODE),    "let bound = int (System.Math.Sqrt(float n))\n");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, COMMENT), "(* Block Comment *)\n");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, COMMENT), "// Line comment\n");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, COMMENT), "//\n");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, CODE),    "let x = 3 + (4 * 5) // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, BLANK),   "     ");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, BLANK),   "\t");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, CODE),    "let bound = int (System.Math.Sqrt(float n))");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, COMMENT), "(* Block Comment *)");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, COMMENT), "// Line comment");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, COMMENT), "//");
		assertLine(new FSharpScanner(), new Line(LANG_FSHARP, CODE),    "let x = 3 + (4 * 5) // with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= "(* Print a list of numbers recursively\n"
			+ "		Written in F#   *)\n"
			+ "let rec printList lst =\n"
			+ "		match lst with\n" 
			+ "		| [] -> ()\n"
			+ "		| h :: t -> \n"
			+ "			printf \"%d\" h\n"
			+ "			printList t\n";

		Line[] expected = {
			new Line(LANG_FSHARP, COMMENT),
			new Line(LANG_FSHARP, COMMENT),
			new Line(LANG_FSHARP, CODE),
			new Line(LANG_FSHARP, CODE),
			new Line(LANG_FSHARP, CODE),
			new Line(LANG_FSHARP, CODE),
			new Line(LANG_FSHARP, CODE),
			new Line(LANG_FSHARP, CODE)
		};
		assertLines(new FSharpScanner(), expected, code);
	}

	@Test
	public void unterminatedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "(*\n\n\n";

		Line[] expected = {
				new Line(LANG_FSHARP, COMMENT),
				new Line(LANG_FSHARP, BLANK),
				new Line(LANG_FSHARP, BLANK)
			};
		assertLines(new FSharpScanner(), expected, code);
	}
}