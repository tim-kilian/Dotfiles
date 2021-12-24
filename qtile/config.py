from typing import List  # noqa: F401

from libqtile import qtile, bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, ScratchPad, DropDown, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from custom import mytree
import nerdfonts as nf
import os, subprocess

mod = "mod4"
terminal = "tilix"


def toggle_focus_floating():
    """Toggle focus between floating window and other windows in group"""

    @lazy.function
    def _toggle_focus_floating(qtile):
        group = qtile.current_group
        switch = "non-float" if qtile.current_window.floating else "float"
        logger.debug(
            f"toggle_focus_floating: switch = {switch}\t current_window: {qtile.current_window}"
        )
        logger.debug(f"focus_history: {group.focus_history}")

        for win in reversed(group.focus_history):
            logger.debug(f"{win}: {win.floating}")
            if switch == "float" and win.floating:
                # win.focus(warp=False)
                group.focus(win)
                return
            if switch == "non-float" and not win.floating:
                # win.focus(warp=False)
                group.focus(win)
                return

    return _toggle_focus_floating


def sortSections(window):
    if "edel" in window.name:
        return "Edel-Optics"
    else:
        return "Other"


def kick_to_next_screen(qtile, direction=1):
    other_scr_index = (qtile.screens.index(qtile.current_screen) + direction) % len(
        qtile.screens
    )
    othergroup = None
    for group in qtile.cmd_groups().values():
        if group["screen"] == other_scr_index:
            othergroup = group["name"]
            break
    if othergroup:
        qtile.move_to_group(othergroup)


def move_to_prev_group(qtile):
    # qtile.move_to_group(othergroup)
    pass


def move_to_next_group(qtile):
    # qtile.move_to_group(othergroup)
    pass


keys = [
    Key([mod], "Left", lazy.layout.left()),
    Key([mod], "Right", lazy.layout.right()),
    Key([mod], "Down", lazy.layout.down()),
    Key([mod], "Up", lazy.layout.up()),
    Key([mod, "shift"], "Left", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "Right", lazy.layout.shuffle_right()),
    Key([mod, "shift"], "Down", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "Up", lazy.layout.shuffle_up()),
    Key([mod, "control"], "Left", lazy.layout.grow_left()),
    Key([mod, "control"], "Right", lazy.layout.grow_right()),
    Key([mod, "control"], "Down", lazy.layout.grow_down()),
    Key([mod, "control"], "Up", lazy.layout.grow_up()),
    Key([mod, "control"], "h", lazy.layout.grow()),
    Key([mod, "control"], "j", lazy.layout.shrink()),
    Key([mod], "Next", lazy.screen.prev_group()),
    Key([mod], "Prior", lazy.screen.next_group()),
    Key([mod, "shift"], "Next", lazy.function(move_to_prev_group)),
    Key([mod, "shift"], "Prior", lazy.function(move_to_next_group)),
    Key([mod], "k", lazy.function(kick_to_next_screen)),
    Key([mod, "shift"], "k", lazy.function(kick_to_next_screen, -1)),
    Key(["mod1"], "Tab", lazy.layout.next()),
    Key(["mod1", "shift"], "Tab", lazy.layout.previous()),
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod, "shift"], "Tab", lazy.prev_layout()),
    Key([mod], "o", lazy.layout.normalize()),
    Key([mod], "s", lazy.layout.sort_windows(sortSections)),
    Key([mod], "space", lazy.layout.toggle_split()),
    Key([mod, "shift"], "space", lazy.layout.rotate()),
    Key([mod, "shift"], "Return", lazy.spawn(terminal)),
    Key([mod, "shift"], "c", lazy.window.kill()),
    Key(["mod1"], "F4", lazy.window.kill()),
    Key([mod], "t", lazy.window.toggle_floating()),
    Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod], "q", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),
    Key(
        [mod],
        "r",
        lazy.spawn(
            "rofi"
            + " -combi-modi window,drun"
            + " -theme android_notification"
            + ' -font "hack 10"'
            + " -show combi"
        ),
    ),
    Key([mod], "p", lazy.spawn('dmenu_run -fn "xft:Roboto:size=13"')),
    Key([mod], "l", lazy.spawn("slock")),
    Key([mod], "n", lazy.spawn("bijiben")),
    Key([mod], "e", lazy.spawn("nemo")),
    Key([mod, "shift"], "e", lazy.spawn("krusader")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("pactl set-sink-volume 0 +5%")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pactl set-sink-volume 0 -5%")),
    Key([], "XF86AudioMute", lazy.spawn("amixer sset Master toggle")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight -inc 10")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -dec 10")),
    Key(
        [],
        "Print",
        lazy.spawn("flameshot gui -p " + os.path.expanduser("~/Pictures/Screenshots/")),
    ),
    Key(
        [mod],
        "Print",
        lazy.spawn(
            "flameshot screen -p " + os.path.expanduser("~/Pictures/Screenshots/")
        ),
    ),
    Key([mod], "c", lazy.group["scratchpad"].dropdown_toggle("term")),
    Key([mod], "h", lazy.group["scratchpad"].dropdown_toggle("htop")),
    Key([mod], "g", lazy.group["scratchpad"].dropdown_toggle("apps")),
    Key([mod], "a", lazy.group["scratchpad"].dropdown_toggle("pavu")),
]

groups = [
    ScratchPad(
        "scratchpad",
        [
            DropDown("term", "alacritty", opacity=0.8),
            DropDown("htop", "alacritty -e htop", opacity=0.8),
            DropDown("apps", "xfce4-appfinder --disable-server", opacity=0.8),
            DropDown("pavu", "pavucontrol", opacity=0.8),
        ],
    ),
    Group(
        "1",
        label=nf.icons["fa_firefox"] + "¹",
        matches=[
            Match(wm_class="firefox"),
        ],
    ),
    Group(
        "2",
        label=nf.icons["mdi_code_array"] + "²",
        layout="tab",
        matches=[
            Match(wm_class="code-oss"),
            Match(wm_class="jetbrains-idea"),
        ],
    ),
    Group(
        "3",
        label=nf.icons["mdi_comment_text"] + "³",
        matches=[Match(func=lambda w: "microsoft teams" in w.get_wm_class())],
    ),
    Group("4", label=nf.icons["dev_terminal"] + "⁴"),
    Group("5", label=nf.icons["mdi_file_document"] + "⁵"),
    Group("6", label=nf.icons["mdi_image"] + "⁶"),
    Group(
        "7",
        label=nf.icons["fa_music"] + "⁷",
        matches=[Match(func=lambda w: "YouTube Music" in w.get_name())],
    ),
    Group("8", label=nf.icons["mdi_xbox_controller"] + "⁸"),
    Group(
        "9",
        label=nf.icons["fa_home"] + "⁹",
        layout="float",
    ),
    Group("0", label=nf.icons["fa_gear"] + "⁰"),
]
# Cool icons: fa_wechat, mdi_email mdi_cloud fae_galery mdi_camcorder_box fa_rocket fa_heart

for i in groups:
    if i.name is "scratchpad":
        continue
    keys.extend(
        [
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
            ),
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name),
            ),
        ]
    )

layouts = [
    layout.MonadTall(
        name="tall",
        border_width=0,
        margin=20,
        margin_on_single=0,
        align=1,
        ratio=0.6,
    ),
    mytree.TreeTab(
        place_right=True,
        panel_width=300,
        bg_color="#24262b",
        inactive_bg="#41434a",
        active_bg="#215578",
        section_fg="#aaaaaa",
        name="tab",
        font="Roboto",
        sections=["Default"],
        padding_y=5,
        section_top=4,
        section_left=16,
    ),
    layout.Max(name="max"),
    layout.Floating(name="float", border_width=0, border_focus="#333333"),
    # Try more layouts by unleashing below layouts.
    # layout.Columns(
    #     num_columns=3, border_width=0, name="Col", margin=8, margin_on_single=0
    # ),
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="sans",
    fontsize=14,
    padding=10,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.TextBox(
                    nf.icons["linux_archlinux"],
                    font="Hack Nerd Font",
                    fontsize=16,
                    foreground="#1793d1",
                    mouse_callbacks={
                        "Button1": lambda: qtile.cmd_simulate_keypress([mod], "g")
                    },
                    padding=14,
                ),
                widget.GroupBox(font="Hack Nerd Font", fontsize=16, padding=3),
                widget.CurrentLayout(),
                # widget.CurrentLayoutIcon(
                #     custom_icon_paths=[
                #         os.path.expanduser("~") + "/.config/qtile/custom/icons"
                #     ],
                #     scale=0.7,
                # ),
                widget.WindowCount(),
                widget.Prompt(),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.WindowName(),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.Clock(
                    format="%H:%M    %d.%m.%Y",
                    mouse_callbacks={
                        "Button1": lambda: qtile.cmd_spawn("gnome-calendar")
                    },
                ),
                widget.Spacer(),
                widget.CheckUpdates(),
                widget.CPU(
                    format=nf.icons["mdi_chip"] + " {load_percent}%",
                    font="Hack Nerd Font",
                    foreground="#EC9A29",
                    fill_color="92140C.3",
                    mouse_callbacks={
                        "Button1": lambda: qtile.cmd_simulate_keypress([mod], "h")
                    },
                ),
                widget.CPUGraph(
                    samples=60,
                    border_width=0,
                    graph_color="#EC9A29",
                    mouse_callbacks={
                        "Button1": lambda: qtile.cmd_simulate_keypress([mod], "h")
                    },
                ),
                widget.Memory(
                    format=nf.icons["mdi_memory"] + " {MemPercent}%",
                    font="Hack Nerd Font",
                    fontsize=14,
                    foreground="#0088cc",
                    mouse_callbacks={
                        "Button1": lambda: qtile.cmd_simulate_keypress([mod], "h")
                    },
                ),
                widget.MemoryGraph(
                    samples=60,
                    border_width=0,
                    graph_color="#0088cc",
                    mouse_callbacks={
                        "Button1": lambda: qtile.cmd_simulate_keypress([mod], "h")
                    },
                ),
                widget.PulseVolume(
                    # emoji=True,
                    foreground="#AC80A0",
                    font="Hack Nerd Font",
                    mouse_callbacks={
                        "Button1": lambda: qtile.cmd_simulate_keypress([mod], "a")
                    },
                ),
                widget.Battery(
                    format="{char} {percent:2.0%}",
                    font="Hack Nerd Font",
                    show_short_text=False,
                    full_char=nf.icons["mdi_battery"],
                    charge_char=nf.icons["mdi_battery_charging"],
                    discharge_char=nf.icons["mdi_battery_50"],
                    unknown_char=nf.icons["mdi_battery_unknown"],
                    empty_char=nf.icons["mdi_battery_outline"],
                    foreground="#758E4F",
                    low_percentage=0.15,
                    low_foreground="#92140C",
                    notify_below=15,
                    update_interval=30,
                ),
                widget.Systray(icon_size=22),
                widget.TextBox(
                    nf.icons["iec_power"],
                    font="Hack Nerd Font",
                    fontsize=16,
                    mouse_callbacks={"Button1": lambda: qtile.cmd_spawn("oblogout")},
                ),
            ],
            24,
            background="#34363d",
            border_color=["#0088cc", "#0088cc", "#0088cc", "#0088cc"],
            border_width=[0, 0, 2, 0],
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.TextBox(
                    nf.icons["linux_archlinux"],
                    font="Hack Nerd Font",
                    fontsize=16,
                    foreground="#1793d1",
                    mouse_callbacks={
                        "Button1": lambda: qtile.cmd_simulate_keypress([mod], "g")
                    },
                    padding=14,
                ),
                widget.GroupBox(font="Hack Nerd Font", fontsize=16, padding=3),
                widget.CurrentLayout(),
                widget.WindowCount(),
                widget.Prompt(),
                widget.WindowTabs(),
                widget.Clock(format="%H:%M"),
                widget.Clock(format="%d.%m.%Y"),
                widget.Spacer(),
                widget.TextBox(
                    nf.icons["iec_power"],
                    font="Hack Nerd Font",
                    fontsize=16,
                    mouse_callbacks={"Button1": lambda: qtile.cmd_spawn("oblogout")},
                ),
            ],
            24,
            background="#34363d",
            border_color=["#0088cc", "#0088cc", "#0088cc", "#0088cc"],
            border_width=[0, 0, 2, 0],
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),
        Match(wm_class="makebranch"),
        Match(wm_class="maketag"),
        Match(wm_class="ssh-askpass"),
        Match(title="branchdialog"),
        Match(title="win0"),
        Match(title="pinentry"),
        Match(wm_class="gnome-calculator"),
        Match(wm_class="bijiben"),
        Match(wm_class="oblogout"),
    ],
    border_width=0,
    border_focus="#333333",
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True


@hook.subscribe.startup_once
def start_once():
    subprocess.call([os.path.expanduser("~") + "/.config/qtile/autostart.sh"])


@hook.subscribe.screen_change
def screen_change(qtile, ev):
    # TODO
    pass


@hook.subscribe.client_managed
def client_managed(window):
    if qtile.current_layout.cmd_sort_windows:
        qtile.current_layout.cmd_sort_windows(sortSections)


@hook.subscribe.focus_change
def focus_change():
    if qtile.current_layout.cmd_sort_windows:
        qtile.current_layout.cmd_sort_windows(sortSections)


wmname = "LG3D"
