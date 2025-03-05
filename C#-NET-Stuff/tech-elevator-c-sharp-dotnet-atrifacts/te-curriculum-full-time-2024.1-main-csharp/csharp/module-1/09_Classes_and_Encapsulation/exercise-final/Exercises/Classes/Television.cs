namespace Exercises.Classes
{
    public class Television
    {
        /// <summary>
        /// Current on/off status of the tv
        /// </summary>
        public bool IsOn { get; private set; }


        /// <summary>
        /// Current selected channel number for the tv. Channels are 3 to 18.
        /// </summary>
        public int CurrentChannel { get; private set; } = 3;


        /// <summary>
        /// Current volume level for the tv. Volume level ranges from 0 to 10.
        /// </summary>
        public int CurrentVolume { get; private set; } = 2;


        /// <summary>
        /// Turns the tv on. Also resets the volume level to 2
        /// </summary>
        public void TurnOn()
        {
            IsOn = true;
            CurrentVolume = 2;
            CurrentChannel = 3;
        }


        /// <summary>
        /// Tuns the tv off.
        /// </summary>
        public void TurnOff()
        {
            IsOn = false;
        }


        /// <summary>
        /// Changes the tv channel, but only if it is on.
        /// </summary>
        /// <param name="newChannel">new channel to turn to</param>
        public void ChangeChannel(int newChannel)
        {
            if (IsOn && newChannel > 3 && newChannel < 18)
            {
                CurrentChannel = newChannel;
            }
        }


        /// <summary>
        /// Increases the Current Channel by 1, only if it is on.
        /// </summary>
        public void ChannelUp()
        {
            if (IsOn)
            {
                if (CurrentChannel == 18)
                {
                    CurrentChannel = 3;
                }
                else
                {
                    CurrentChannel++;
                }
            }
        }


        /// <summary>
        /// Increases the Current Channel by 1, only if it is on.
        /// </summary>
        public void ChannelDown()
        {
            if (IsOn)
            {
                if (CurrentChannel == 3)
                {
                    CurrentChannel = 18;
                }
                else
                {
                    CurrentChannel--;
                }
            }
        }


        /// <summary>
        /// Raises the volume. It won't raise it past 10
        /// </summary>
        public void RaiseVolume()
        {
            if (IsOn && CurrentVolume < 10)
            {
                CurrentVolume++;
            }
        }


        /// <summary>
        /// Lowers the volume. It won't lower it below 0
        /// </summary>
        public void LowerVolume()
        {
            if (IsOn && CurrentVolume > 0)
            {
                CurrentVolume--;
            }
        }
    }
}
