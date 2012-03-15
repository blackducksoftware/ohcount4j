package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class AssemblyScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new AssemblyScanner(), new Line(Language.ASM, BLANK),   "\n");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, BLANK),   "     \n");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, BLANK),   "\t\n");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, CODE),    "mov(ax, bx);\n");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, COMMENT), "/* Block Comment */\n");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, COMMENT), "# Line comment\n");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, COMMENT), "#\n");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, CODE),    "mov ax, bx    ;we move bx into ax\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new AssemblyScanner(), new Line(Language.ASM, BLANK),   "     ");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, BLANK),   "\t");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, CODE),    "mov(ax, bx);");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, COMMENT), "/* Block Comment */");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, COMMENT), "# Line comment");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, COMMENT), "#");
		assertLine(new AssemblyScanner(), new Line(Language.ASM, CODE),    "mov ax, bx    ;we move bx into ax");
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
			new Line(Language.ASM, COMMENT),
			new Line(Language.ASM, BLANK),
			new Line(Language.ASM, COMMENT),
			new Line(Language.ASM, COMMENT),
			new Line(Language.ASM, CODE),
			new Line(Language.ASM, COMMENT),
			new Line(Language.ASM, CODE),
			new Line(Language.ASM, COMMENT),
			new Line(Language.ASM, CODE),
			new Line(Language.ASM, CODE),
			new Line(Language.ASM, CODE),
			new Line(Language.ASM, BLANK)
		};
		assertLines(new AssemblyScanner(), expected, code);
	}

}
