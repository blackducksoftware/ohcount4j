// Program 11.6: A nicer sine wave
import java.applet.Applet;
import java.awt.Graphics;

public class SineApplet2 extends Applet {

	@Override
	public void paint(Graphics g) {

		int i, j1, j2;

		j1 = yvalue(0);
		for (i = 0; i < size().width; i++) {
			j2 = yvalue(i + 1);
			g.drawLine(i, j1, i + 1, j2);
			j1 = j2;
		}

	}

	// Given the xpoint we're given calculate the Cartesian equivalent
	private int yvalue(int ivalue) {

		double xmin = -10.0;
		double xmax = 10.0;
		double ymin = -1.0;
		double ymax = 1.0;
		double x, y;
		int jvalue;

		x = (ivalue * (xmax - xmin) / (size().width - 1)) + xmin;

		// Take the sine of that x
		y = Math.sin(x);

		// Scale y into window coordinates
		jvalue = (int) ((y - ymin) * (size().height - 1) / (ymax - ymin));

		/*
		 * Switch jvalue from Cartesian coordinates to computer graphics
		 * coordinates
		 */
		jvalue = size().height - jvalue;

		return jvalue;

	}

}