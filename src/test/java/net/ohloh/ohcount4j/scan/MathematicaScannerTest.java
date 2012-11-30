package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class MathematicaScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, BLANK),   "\n");
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, BLANK),   "     \n");
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, BLANK),   "\t\n");
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, CODE),    "While[(i<=Length[a]),x=a[[i]];\n");
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, COMMENT), "(* Block comment *)\n");
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, CODE),    "While[(i<=Length[a]),x=a[[i]]; (* with comment *)\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, BLANK),   "     ");
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, BLANK),   "\t");
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, CODE),    "While[(i<=Length[a]),x=a[[i]];");
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, COMMENT), "(* Block comment *)");
		assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, CODE),    "While[(i<=Length[a]),x=a[[i]]; (* with comment *)");
	}

	@Test
	public void sampleTest() {
		String code
			= "(* Part of a sample program\n"
			+ "		written in Mathematica\n"
			+ "\t\n"
			+ "		(* Nested Comment *)\n"
			+ "				*)\n"
			+ "While[(i<=Length[a]),x=a[[i]];\n"
			+ "		While[((i<=Length[a])&&(a[[i]]==x)),cnt+=1;i+=1];\n"
			+ "			ls=Append[ls,cnt];\n"
			+ "			cnt=0]\n"
			+ "a\n"
			+ "ls;\n";

		Line[] expected = {
			new Line(Language.MATHEMATICA, COMMENT),
			new Line(Language.MATHEMATICA, COMMENT),
			new Line(Language.MATHEMATICA, BLANK),
			new Line(Language.MATHEMATICA, COMMENT),
			new Line(Language.MATHEMATICA, COMMENT),
			new Line(Language.MATHEMATICA, CODE),
			new Line(Language.MATHEMATICA, CODE),
			new Line(Language.MATHEMATICA, CODE),
			new Line(Language.MATHEMATICA, CODE),
			new Line(Language.MATHEMATICA, CODE),
			new Line(Language.MATHEMATICA, CODE)
		};
		assertLines(Language.MATHEMATICA, expected, code);
	}

	@Test
	public void unterminatedNestedCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "(*\n(* *)\n\n\n";

		Line[] expected = {
				new Line(Language.MATHEMATICA, COMMENT),
				new Line(Language.MATHEMATICA, COMMENT),
				new Line(Language.MATHEMATICA, BLANK),
				new Line(Language.MATHEMATICA, BLANK)
			};
		assertLines(Language.MATHEMATICA, expected, code);
	}
}
