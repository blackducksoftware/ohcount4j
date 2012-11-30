package net.ohloh.ohcount4j.scan;

import static org.testng.AssertJUnit.assertEquals;
import static org.testng.Assert.fail;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.scan.Scanner;
import net.ohloh.ohcount4j.scan.Line;
import net.ohloh.ohcount4j.scan.TestLineHandler;

public class BaseScannerTest {


	protected void assertLine(Language language, Line expected, String code) {
		assertLines(language, new Line[] {expected}, code);
	}

	protected void assertLines(Language language, Line[] expected, String code) {
		TestLineHandler h = new TestLineHandler();
		Scanner scanner;

		try {
			scanner = language.makeScanner();
		} catch (OhcountException e) {
			fail("Could not instantiate scanner", e.getCause());
			return;
		}

		scanner.scan(code, h);

		assertEquals(expected.length, h.getLines().size());

		for (int i=0; i<expected.length; i++) {
			Line line = h.getLines().get(i);
			String msg = String.format("at line %1$d: %2$s", i+1, line.getContent());
			assertEquals(msg, expected[i].language, line.language);
			assertEquals(msg, expected[i].entity, line.entity);
		}
	}

}
