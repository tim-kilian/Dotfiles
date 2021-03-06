import math
from collections import namedtuple

from libqtile.layout.xmonad import MonadTall

class MonadThreeCol(MonadTall):
    """Emulate the behavior of XMonad's ThreeColumns layout.
    A layout similar to tall but with three columns. With an ultra wide display
    this layout can be used for a huge main window - ideally at the center of the
    screen - and up to six reasonable sized secondary windows.
    Main-Pane:
    A main pane that contains a single window takes up a vertical portion of
    the screen_rect based on the ratio setting. This ratio can be adjusted with
    the ``cmd_grow_main`` and ``cmd_shrink_main`` or, while the main pane is in
    focus, ``cmd_grow`` and ``cmd_shrink``. The main pane can also be centered.
    ::
        ---------------------------    ---------------------------
        |           |      |      |    |      |           |      |
        |           |      |      |    |      |           |      |
        |           |      |      |    |      |           |      |
        |           |      |      |    |      |           |      |
        |           |      |      |    |      |           |      |
        |           |      |      |    |      |           |      |
        ---------------------------    ---------------------------
    Secondary-panes:
    Occupying the rest of the screen_rect are one or more secondary panes.  The
    secondary panes will be divided into two columns and share the vertical space
    of each column. However they can be resized at will with the ``cmd_grow`` and
    ``cmd_shrink`` methods. The other secondary panes will adjust their sizes to
    smoothly fill all of the space.
    ::
        ---------------------------    ---------------------------
        |           |      |      |    |           |______|      |
        |           |______|      |    |           |      |      |
        |           |      |______|    |           |      |______|
        |           |______|      |    |           |      |      |
        |           |      |      |    |           |______|      |
        |           |      |      |    |           |      |      |
        ---------------------------    ---------------------------
    Panes can be moved with the ``cmd_shuffle_up`` and ``cmd_shuffle_down``
    methods. As mentioned the main pane is considered the top of the stack;
    moving up is counter-clockwise and moving down is clockwise. A secondary
    pane can also be promoted to the main pane with the ``cmd_swap_main``
    method.
    Normalizing/Resetting:
    To restore all secondary client windows to their default size ratios
    use the ``cmd_normalize`` method.
    To reset all client windows to their default sizes, including the primary
    window, use the ``cmd_reset`` method.
    Maximizing:
    To maximized a client window simply use the ``cmd_maximize`` on a focused
    client.
    """

    defaults = [
        ("main_centered", True, "Place the main pane at the center of the screen"),
    ]

    def __init__(self, **config):
        MonadTall.__init__(self, **config)
        self.add_defaults(MonadThreeCol.defaults)
        self.new_client_position = "top"

    def _configure_specific(self, client, screen_rect, border_color, index):
        """Specific configuration for xmonad three columns."""
        if index == 0:
            self._configure_main(client)
        elif self._get_column(index - 1).name == "left":
            self._configure_left(client, index)
        else:
            self._configure_right(client, index)

    def _configure_main(self, client):
        """Configure the main client"""
        width = self._get_main_width()
        height = self.screen_rect.height
        left = self.screen_rect.x
        top = self.screen_rect.y

        if self.main_centered and len(self.clients) > 2:
            left += (self.screen_rect.width - width) // 2

        self._place_client(client, left, top, width, height)

    def _configure_left(self, client, index):
        """Configure the left column"""
        width = self._get_secondary_widths()[0]
        height = self._get_secondary_height(index)
        left = self.screen_rect.x
        top = self.screen_rect.y + self._get_relative_sizes_above(index)

        if not self.main_centered or len(self.clients) == 2:
            left += self._get_main_width()

        self._place_client(client, left, top, width, height)

    def _configure_right(self, client, index):
        """Configure the right column"""
        widths = self._get_secondary_widths()
        height = self._get_secondary_height(index)
        left = self.screen_rect.x + widths[0] + self._get_main_width()
        top = self.screen_rect.y + self._get_relative_sizes_above(index)

        self._place_client(client, left, top, widths[1], height)

    def _get_main_width(self):
        """Calculate the main client's width"""
        return int(self.screen_rect.width * self.ratio)

    def _get_secondary_widths(self):
        """Calculate secondary clients' widths"""
        width = self.screen_rect.width - self._get_main_width()
        if len(self.clients) == 2:
            return [width, 0]

        return self._split_integer(width, 2)

    def _get_secondary_height(self, index):
        """Return the height of the provided index"""
        return self.relative_sizes[index - 1]

    def _get_relative_sizes_above(self, index):
        """Return the sum of the heights of all clients above the provided index"""
        column = self._get_column(index - 1)
        return sum(self.relative_sizes[column.start:index - 1])

    def _place_client(self, client, left, top, width, height):
        """Place a client on the screen
        Will prevent double margins by applying east and south margins only
        when the client is the rightmost or the bottommost window.
        """
        rightmost = left + width - self.screen_rect.x >= self.screen_rect.width
        bottommost = top + height - self.screen_rect.y >= self.screen_rect.height
        margin = [self.margin] * 4
        if not rightmost:
            margin[1] = 0
        if not bottommost:
            margin[2] = 0

        client.place(
            left,
            top,
            width - 2 * self.border_width,
            height - 2 * self.border_width,
            self.border_width,
            self.border_focus if client.has_focus else self.border_normal,
            margin=margin,
        )

    def cmd_normalize(self, redraw=True):
        """Evenly distribute screen-space among secondary clients"""
        if self.screen_rect is not None:
            self.relative_sizes = []

            height = self.screen_rect.height
            left, right = self._get_columns()

            if left.count > 0:
                self.relative_sizes += self._split_integer(height, left.count)
            if right.count > 0:
                self.relative_sizes += self._split_integer(height, right.count)

        if redraw:
            self.group.layout_all()
        self.do_normalize = False

    def cmd_swap_main(self):
        """Swap current window to main pane"""
        self.cmd_swap(self.clients.current_client, self.clients[0])

    def _maximize_secondary(self):
        """Maximize the focused secondary pane"""
        focused = self.focused - 1
        column = self._get_column(focused)
        if column.count == 1:
            return

        max_height = self.screen_rect.height - ((column.count - 1) * self.min_secondary_size)
        for i in range(column.start, column.end):
            self.relative_sizes[i] = max_height if i == focused else self.min_secondary_size

    def _grow_secondary(self, amt):
        """Grow the focused client in the secondary pane"""
        self._resize_secondary(amt)

    def _shrink_secondary(self, amt):
        """Shrink the focused client in the secondary pane"""
        self._resize_secondary(-amt)

    def _resize_secondary(self, amt):
        """Resize the focused secondary client
        If amt is positive, the client will grow. Conversely, if it's negative,
        the client will shrink. All other clients in the same column will get
        grown/shrunk so to accommodate the new height.
        """
        focused = self.focused - 1
        column = self._get_column(focused)

        if column.count == 1:
            return

        # Resizing is accomplished by doing the following:
        # - calculate how much each client in the column must shrink/grow
        #   so that the focused window can grow/shrink.
        # - iterate over all clients in the column and change their height
        #   (grow or shrink) as long as they can still be resized (both main
        #   and secondary windows).
        min_height = self.min_secondary_size
        idx = column.start
        step = amt // (column.count - 1)
        visited = 0
        while amt != 0:
            if idx != focused:
                focused_new_height = self.relative_sizes[focused] + step
                new_height = self.relative_sizes[idx] - step
                if focused_new_height >= min_height and new_height >= min_height:
                    self.relative_sizes[focused] += step
                    self.relative_sizes[idx] -= step
                    amt -= step
                    visited += 1

            idx += 1
            if idx == column.end:
                if visited == 0:
                    break

                idx = column.start
                visited = 0

        self.group.layout_all()

    def _get_column(self, index):
        """Get the column containing the provided index"""
        left, right = self._get_columns()
        return left if index < left.count else right

    def _get_columns(self):
        """Get all columns"""
        Column = namedtuple("Column", "name count start end")
        clients = len(self.clients) - 1
        clients = [clients // 2 + clients % 2, clients // 2]
        return [
            Column(
                name="left",
                count=clients[0],
                start=0,
                end=clients[0],
            ),
            Column(
                name="right",
                count=clients[1],
                start=clients[0],
                end=clients[0] + clients[1],
            )
        ]

    def _split_integer(self, value, parts):
        """Divide an integer into equal parts and distribute the remainder"""
        result = [value // parts] * parts
        for i in range(value % parts):
            result[i] += 1
        return result

    def info(self):
        left, right = self._get_columns()
        d = MonadTall.info(self)
        d.update(
            secondary=dict(
                left=d["clients"][1:left.end + 1] if left.count > 0 else [],
                right=d["clients"][right.start + 1:] if right.count > 0 else [],
            )
        )
        return d