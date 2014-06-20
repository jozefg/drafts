---
title: Teaching Python with a Raspberry Pi
---

This last week I've been volunteering at a summer camp. This camp is
aimed at kids ages 8 to 12 and teaches the basics of Python!

I wanted to write down some of my thoughts and experiences on the
whole process.

## The Curriculum

The curriculum for the camp was based around 3 key components

 - Python
 - Raspberry Pis
 - Minecraft

The camp was spread over 4 days, each 3 hours. Each day introduced
more of Python with more sophisticated programs. Each program actually
interacted with minecraft, building structures, modifying worlds, and
doing cool visible things. We'll talk later about how this was
possible.

Going into the camp, the expected schedule was something like

 1. Introduce the Pi, show how to run Python scripts from terminal
 2. Introduce the basics of Python, mostly variables and conditionals
 3. Apply these basics with the minecraft API
 4. Introduce loops, apply this with a few more advanced programs

In hindsight, this curriculum was a tad bit unrealistic, but what
curriculum isn't.

## The Staff

This was the first time the camp was run, so the staff was a little
inexperienced.

I was the only person familiar with programming but had never taught
young children before, and the two payed staff members were used to
teaching basic science camps but had never taught anything CS-ish.
This meant that a lot of this was a learning experience for us as much
as the kids.

## The Children

The camp was over-capacity with 14 children. None of them has ever
programmed before per-se. But two had done some basic HTML layout and
one 10 year old was quite familiar with unix after 2 years of running
various Linux distributions (I was impressed).

The unfortunate fact was that since the camp was marketed as teaching
with Minecraft, a lot of the kids just showed up to play
Minecraft. This was anticipated but still a little saddening.

## Day 1

On day 1, we get everyone set up with their own Pi, we also included

 - A cheap monitor
 - A very cheap mouse
 - A keyboard

Getting this all set up for 14 kids was a lot smoother than
anticipated. The only hitch was the SD cards we'd purchased were a lot
cheaper than anticipated so we burned through maybe 5 cards that we
just couldn't get a Pi to boot with.

We got everyone successfully to a desktop in about 30 minutes.

The Pis were running a custom operating system called Raspbian. This
OS is very verbose during boot time and shows the entire log from
booting up rather than just displaying an innocent little loading
graphic.

Quite a few of the kids were curious about what was going on so we
explained how little about how OS's work. It was pretty awesome to see
kids being interested in what steps a kernel went through.

Sadly I'm not a super knowledgeable person when it comes to OS's. In light
of this I've ordered a book or two on the subject, something that's
been on my todo list for a while now. I should be better prepared
for questions next time.

Now once we got everyone up and running we had people order 2
programs, LXTerminal and Minecraft. This is when we had some fun
trying to explain what exactly a terminal is.

I eventually started simply saying

> LXTerminal is a program that let's you run other programs. It's like
> a text interface so that you can do what you normally do by clicking
> with typing.
>
> Almost all Unix computers, like OS X and Raspbian, have the same way
> of entering stuff into terminals.

From here we had everyone run `cd play`. Luckily a group of volunteer
engineers had sat down and written a bunch of programs to do various
things in Minecraft. The first one everyone started with just built a
grid of stone blocks.

We then started explaining how to run things with the `python`
program. This turned out to be a bit more of a struggle than
anticipated since typing and spelling are more difficult than
anticipated.

We had a lot of people doing things like

     $ pyton grid.py
     $ pythongrid.py
     $ grid.py
     $ python grid.py # Finally

I really wish we had an overhead project to show everyone written
examples on a teacher machine. This was a big problem as time went on,
simply saying things out loud is not a sufficient method for
communicating about programs.

Now, once this ran there was a satisfying "Whoooaaaa" when everyone
saw that this command had modified the game right before their eyes!

Some people quickly started trying to use this to speed up their
building by automatically creating walls for themselves rather than
doing it by hand. This was exactly the response we were looking for
and it was clear this was starting to spark some interest in
programming.

Finally we had everyone open up IDLE. We used IDLE for all our editing
purposes for exactly two reasons

 1. It's dead simple to use
 2. It's preinstalled

Everyone opened up `grid.py` and had a look at the source code. The
code for `grid.py` was roughly

``` python
    import minecraft
    import block

    mc = minecraft.Minecraft.create() # Our connection to Minecraft

    def buildWall(size, type = block.STONE):
        pos = mc.player.getPos()
        pos.x += 3

        for x in range(0, size):
            for y in range(0, size):
                mc.setBlock(pos.x + x, pos.y + y, pos.z, type) # Set block at these coordinates to type

    if __name__ = "__main__":
        buildWall(5)
```

We get a pretty nice high level API to minecraft, and the code is
quite simple. Keep in mind, we have taught exactly 0 python at this
point.

Next we explained that we could change `buildWall(5)` to
`buildWall(6)` and our program would make a bigger wall! Again an
overhead was sorely missed at this point since it was very hard to
explain exactly where we were talking about, even in such small code.

Most people than started modifying the code trying to build as big a
wall as possible. This was also the point at which our first syntax
errors started up.

Since I was the only person in the room who
understood what they meant there was a fair bit of running around. I
have to give a lot of credit to the two staff members who essentially
learned the basics of Python syntax by me yelling it to them across
the room!

`grid.py` also included some code to generate a grid with different
blocks. This was another huge success since kids could try to spell
different words in their grid of blocks. I've omitted it from the
above snippet since frankly I don't remember it.

This took up most of the first day, since everyone also got a 30
minute snack breaks (don't you miss snack breaks?).

## Day 2

The next day we were actually aiming to teach some programming! This
had a script written already by the engineers who'd written the
code we'd used yesterday, but upon consulting the script I found

 1. Teach variables
 2. Explain what a value is
 3. Explain if's
 4. Questions

Uh oh. So I ended up writing a few notes down the night before, we
didn't have access to any sort of projector so a lot of my
explanations consisted of scribbling on a giant (2' by 3') post it
note.

This had distinctly mixed results. As I'd expected
 
