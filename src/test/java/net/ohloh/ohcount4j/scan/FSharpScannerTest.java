package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class FSharpScannerTest extends AbstractBaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.FSHARP, new Line(Language.FSHARP, BLANK),   "\n");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, BLANK),   "     \n");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, BLANK),   "\t\n");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, CODE),    "let bound = int (System.Math.Sqrt(float n))\n");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "(* Block Comment *)\n");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "// Line comment\n");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "//\n");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, CODE),    "let x = 3 + (4 * 5) // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.FSHARP, new Line(Language.FSHARP, BLANK),   "     ");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, BLANK),   "\t");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, CODE),    "let bound = int (System.Math.Sqrt(float n))");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "(* Block Comment *)");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "// Line comment");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "//");
		assertLine(Language.FSHARP, new Line(Language.FSHARP, CODE),    "let x = 3 + (4 * 5) // with comment");
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
			new Line(Language.FSHARP, COMMENT),
			new Line(Language.FSHARP, COMMENT),
			new Line(Language.FSHARP, CODE),
			new Line(Language.FSHARP, CODE),
			new Line(Language.FSHARP, CODE),
			new Line(Language.FSHARP, CODE),
			new Line(Language.FSHARP, CODE),
			new Line(Language.FSHARP, CODE)
		};
		assertLines(Language.FSHARP, expected, code);
	}

	@Test
	public void unterminatedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "(*\n\n\n";

		Line[] expected = {
				new Line(Language.FSHARP, COMMENT),
				new Line(Language.FSHARP, BLANK),
				new Line(Language.FSHARP, BLANK)
			};
		assertLines(Language.FSHARP, expected, code);
	}
}
