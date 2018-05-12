# sdl2-sprite

Create a horizontal sprite like this:

![](img/blue-fish-sprite.png)

The size of the intended result image is 220x130, so provide that:

    sdl2-sprite blue-fish.png 220x130 --fps 15

This will open a window animating that sprite:

![](img/blue-fish.gif)

The animation repeats indefinitely at the desired framerate.

## How to make sprites in Inkscape

Choose the size of your intended result e.g. 220x130 and then setup
the grid in Inkscape to show that size:

![](img/inkscape-doc-properties.png)

It'll look like this, which is handy:

![](img/inkscape-grid.png)

When you want to make a new frame, use `Ctrl-D` to duplicate the
selection and use the Transform tool to move it exactly 220 pixels to
the right and hit Apply.

![](img/inkscape-transform.png)

Finally, export your image with the right "Image size" - make sure the
size is 130 so that the width scales right:

![](img/inkscape-export.png)

Now you're good to go!
