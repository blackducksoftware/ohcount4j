package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class AssemblyScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, BLANK),   "\n");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, BLANK),   "     \n");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, BLANK),   "\t\n");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, CODE),    "mov(ax, bx);\n");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "/* Block Comment */\n");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "# Line comment\n");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "#\n");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, CODE),    "mov ax, bx    ;we move bx into ax\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, BLANK),   "     ");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, BLANK),   "\t");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, CODE),    "mov(ax, bx);");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "/* Block Comment */");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "# Line comment");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "#");
		assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, CODE),    "mov ax, bx    ;we move bx into ax");
	}

	@Test
	public void sampleTest() {
		String code
			= "/* Simple Assembly Program\n"
			+ "\t\n"
			+ "*/\n"
			+ "# load $t3\n"
			+ "lb	$t3, ($t0)\n"
			+ "; store contents of $t3\n"
			+ "sb	$t3, ($t1)\n"
			+ "! decrement $t2\n"
			+ "sub	$t2, $t2, 1\n"
			+ "add	$t0, $t0, 1	; increment $t0\n"
			+ "add	$t1, $t1, 1	; increment $t1\n"
			+ "\n";

		Line[] expected = {
			new Line(Language.ASSEMBLY, COMMENT),
			new Line(Language.ASSEMBLY, BLANK),
			new Line(Language.ASSEMBLY, COMMENT),
			new Line(Language.ASSEMBLY, COMMENT),
			new Line(Language.ASSEMBLY, CODE),
			new Line(Language.ASSEMBLY, COMMENT),
			new Line(Language.ASSEMBLY, CODE),
			new Line(Language.ASSEMBLY, COMMENT),
			new Line(Language.ASSEMBLY, CODE),
			new Line(Language.ASSEMBLY, CODE),
			new Line(Language.ASSEMBLY, CODE),
			new Line(Language.ASSEMBLY, BLANK)
		};
		assertLines(Language.ASSEMBLY, expected, code);
	}

}
