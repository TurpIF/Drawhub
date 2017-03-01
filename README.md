# Drawhub
Haskell application to draw 8-bit images on Github contribution table

The application take an image and reduce it size to fit in the 52 (weeks) x 7
(days) activities calendar of github. Then, it reduce the colors to get only
5 shades of green (including white) and print the image or generate the
corresponding git repository with dummy commits.

You still have to push the repository on your github account. Also, as the
activity shades are computed from the maximum of activities per day, if you do
any activities by yourself, you may decolorized generated images. So, I
recommand you to use a dummy account for this.

```
Drawhub - Draw on Github

Usage: Drawhub COMMAND
Draw an image on Github activities calendar

Available options:
  -h,--help                Show this help text

Available commands:
  image                    Transform an image to a preview
  commit                   Create a new repository with commits representing the
                           image

Usage: Drawhub image (-i|--input ARG) (-o|--output ARG)
  Transform an image to a preview

Available options:
  -i,--input ARG           Input path of image to process
  -o,--output ARG          Output path of generated image
  -h,--help                Show this help text


Usage: Drawhub commit (-i|--input ARG) (-o|--output ARG) (-m|--mail ARG)
                      (-d|--start-day ARG) [-b|--bare]
  Create a new repository with commits representing the image

Available options:
  -i,--input ARG           Input path of image to process
  -o,--output ARG          Output path of git repository
  -m,--mail ARG            Mail of the committer (should be same as the Github
                           account's
  -d,--start-day ARG       Start day of commits (should be a sunday)
                           (yyyy-mm-dd)
  -b,--bare                Create a bare git repository
  -h,--help                Show this help text
```


