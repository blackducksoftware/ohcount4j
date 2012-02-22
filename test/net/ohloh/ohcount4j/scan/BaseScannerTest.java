package net.ohloh.ohcount4j.scan;

import static org.testng.AssertJUnit.assertEquals;

import net.ohloh.ohcount4j.scan.Scanner;
import net.ohloh.ohcount4j.scan.Line;

public class BaseScannerTest {


	protected void assertLine(Scanner scanner, Line expected, String code) {
		assertLines(scanner, new Line[] {expected}, code);
	}

	protected void assertLines(Scanner scanner, Line[] expected, String code) {
		TestLineHandler h = new TestLineHandler();
		scanner.scan(code, h);

		assertEquals(expected.length, h.getLines().size());

		for (int i=0; i<expected.length; i++) {
			Line line = h.getLines().get(i);
			assertEquals(expected[i].language, line.language);
			assertEquals(expected[i].entity, line.entity);
		}
	}

}