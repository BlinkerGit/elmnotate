# Elmnotate

Elmnotate is an [electron](https://electronjs.org) app written in
[elm](http://elm-lang.org) for image annotation.  It can capture any number of
text labels with or without an associated geometry per image.

## Usage

![Screencast](https://gist.github.com/rubysolo/62f88ea08d61171fd150d679ddf850f5/raw/82291170991dc29835ef99cca4d486e96d91eae8/elmnotate.gif)

### Scenario 1 : Image Classification

Drag and drop a text file containing URLs for your images onto the drop zone
in the app.  Define the labels for your image dataset.  For each image, type
the value into the text field for the label and click the "next" button to
advance to the next image.  When all the images are labeled, click the
"Download" button to generate a JSON file containing the data.

### Scenario 2 : Object Detection

Drag and drop a JSON file containing URLs and any existing labels or shapes
onto the drop zone in the app.  Define rectangles (defined by two points) or
quads (defined by four points) for your image dataset.  For each image, select
the appropriate shape and then click on the image to add points to the shape.
Points can be dragged into place for fine-tuning.

## Output

After all the images are annotated (or your fingers are tired), click the
"Download" button, and Elmnotate will generate a JSON file which is formatted
as a list of objects shaped like:

```json
{
  "url": "https://url/to/image",
  "shapes": [{
    "label": "eye",
    "quad": [734, 655, 891, 667, 846, 734, 692, 725]
  }],
  "labels": {
    "species": "dog"
  }
}
```

## Installing Dependencies

For the Javascript dependencies, run:

`npm install`

Elmnotate requires [elm](http://elm-lang.org),
[elm-github-install](https://github.com/gdotdesign/elm-github-install)
(to install native packages from github), and [electron](https://electronjs.org).
Install each of these per their instructions, then run:

`elm install`

## Developing

`npm run watch`

then make changes to the code, and the app will recompile and reload.

## Packaging

To build a native app for your platform, run:

`npm run elm:make`
`npm run package`

To cross-compile, there are also platform-specific scripts defined for
`package-mac`, `package-win`, `package-linux`.

## Contributing

Pull requests welcome!  For large changes, please open an issue beforehand
to discuss.
